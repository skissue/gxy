#!/usr/bin/env python3
# coding=utf-8
"""
LRC to ELRC converter using Qwen3ForcedAligner.

Phase 1: LRC parser with --lrc CLI flag.
"""

import argparse
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Tuple

import numpy as np
import soundfile as sf
import torch

from qwen_asr import Qwen3ForcedAligner


@dataclass
class LrcLine:
    """A single LRC lyric line."""
    start_s: float
    text: str
    raw: str
    idx: int


@dataclass
class AudioSegment:
    """An audio segment for alignment."""
    wav: np.ndarray
    sr: int
    offset_s: float
    line: LrcLine


@dataclass
class AlignedWord:
    """A word with absolute timestamps."""
    text: str
    start_s: float
    end_s: float


@dataclass
class AlignedLine:
    """An aligned lyric line with word-level timestamps."""
    line: LrcLine
    words: List[AlignedWord]


# Metadata tags to recognize
METADATA_TAGS = {"ti", "ar", "al", "by", "offset"}

# Pattern for timestamp: [mm:ss.xx] or [mm:ss:xx]
TIMESTAMP_PATTERN = re.compile(r"\[(\d+):(\d+)[.:](\d+)\]")

# Pattern for metadata: [tag:value]
METADATA_PATTERN = re.compile(r"^\[([a-z]+):([^\]]*)\]$", re.IGNORECASE)


def parse_timestamp(match: re.Match) -> float:
    """Convert timestamp match to seconds."""
    mm = int(match.group(1))
    ss = int(match.group(2))
    cs_str = match.group(3)
    # Handle both centiseconds (2 digits) and milliseconds (3 digits)
    if len(cs_str) == 2:
        cs = int(cs_str) / 100.0
    else:
        cs = int(cs_str) / 1000.0
    return mm * 60 + ss + cs


def parse_lrc(filepath: Path) -> Tuple[Dict[str, str], List[LrcLine]]:
    """
    Parse an LRC file.

    Returns:
        Tuple of (metadata dict, list of LrcLine sorted by start_s)
    """
    metadata: Dict[str, str] = {}
    lines: List[LrcLine] = []
    idx = 0

    with open(filepath, "r", encoding="utf-8") as f:
        for raw_line in f:
            raw = raw_line.rstrip("\n\r")

            # Check for metadata tag (single tag on its own line)
            meta_match = METADATA_PATTERN.match(raw.strip())
            if meta_match:
                tag = meta_match.group(1).lower()
                value = meta_match.group(2).strip()
                if tag in METADATA_TAGS:
                    metadata[tag] = value
                    continue

            # Find all timestamps at the start of the line
            timestamps: List[float] = []
            pos = 0
            for match in TIMESTAMP_PATTERN.finditer(raw):
                if match.start() == pos:
                    timestamps.append(parse_timestamp(match))
                    pos = match.end()
                else:
                    break

            if not timestamps:
                # No timestamps found, skip this line
                continue

            # Extract text after all timestamps
            text = raw[pos:]

            # Emit a LrcLine for each timestamp
            for ts in timestamps:
                lines.append(LrcLine(start_s=ts, text=text, raw=raw, idx=idx))
                idx += 1

    # Apply offset if present (offset is in milliseconds)
    if "offset" in metadata:
        try:
            offset_ms = int(metadata["offset"])
            offset_s = offset_ms / 1000.0
            for line in lines:
                line.start_s += offset_s
        except ValueError:
            pass

    # Stable sort by start_s
    lines.sort(key=lambda x: (x.start_s, x.idx))

    return metadata, lines


def load_audio(filepath: Path, mono: bool = True) -> Tuple[np.ndarray, int]:
    """
    Load audio file using soundfile.

    Args:
        filepath: Path to audio file.
        mono: If True, convert to mono. If False, keep original channels.

    Returns:
        Tuple of (waveform as float32 ndarray, sample rate)
        If mono=True: shape (samples,)
        If mono=False: shape (samples, channels)
    """
    if not filepath.exists():
        raise FileNotFoundError(f"Audio file not found: {filepath}")

    try:
        wav, sr = sf.read(filepath, dtype="float32")
    except sf.SoundFileError as e:
        raise RuntimeError(f"Unsupported audio format or corrupted file: {filepath}: {e}")

    if mono and wav.ndim == 2:
        wav = wav.mean(axis=1)

    return np.asarray(wav, dtype=np.float32), int(sr)


def separate_vocals_demucs(wav: np.ndarray, sr: int, device: str) -> Tuple[np.ndarray, int]:
    """
    Separate vocals from audio using Demucs.

    Args:
        wav: Audio waveform, shape (samples,) or (samples, channels).
        sr: Sample rate.
        device: Device for inference (e.g., "cuda:0", "cpu").

    Returns:
        Tuple of (vocals as mono float32 ndarray, sample rate - unchanged)
    """
    try:
        from demucs.pretrained import get_model
        from demucs.apply import apply_model
        import torchaudio
    except ImportError as e:
        raise RuntimeError(
            "Demucs is not installed. Install with: pip install demucs"
        ) from e

    if wav.ndim == 1:
        wav_ch = np.stack([wav, wav], axis=0)
    elif wav.ndim == 2:
        wav_ch = wav.T
        if wav_ch.shape[0] == 1:
            wav_ch = np.repeat(wav_ch, 2, axis=0)
    else:
        raise ValueError(f"Unexpected wav shape: {wav.shape}")

    model = get_model("htdemucs")
    model_sr = model.samplerate

    wav_t = torch.from_numpy(np.asarray(wav_ch, dtype=np.float32))

    if sr != model_sr:
        wav_t = torchaudio.functional.resample(wav_t, sr, model_sr)

    wav_t = wav_t.unsqueeze(0)
    model.to(device)

    with torch.inference_mode():
        stems = apply_model(model, wav_t.to(device), progress=True)

    vocal_idx = model.sources.index("vocals")
    vocals = stems[0, vocal_idx]
    vocals_mono = vocals.mean(dim=0).cpu().numpy().astype(np.float32)

    del model, stems
    if torch.cuda.is_available() and str(device).startswith("cuda"):
        torch.cuda.empty_cache()

    return vocals_mono, int(model_sr)


def slice_audio(
    wav: np.ndarray,
    sr: int,
    lines: List[LrcLine],
    pad: float = 0.2,
    min_seg: float = 0.5,
) -> List[AudioSegment]:
    """
    Slice audio into segments for each non-empty lyric line.

    Args:
        wav: Mono audio waveform.
        sr: Sample rate.
        lines: Parsed LRC lines sorted by start_s.
        pad: Padding in seconds around each segment.
        min_seg: Minimum segment duration in seconds.

    Returns:
        List of AudioSegment for non-empty lines.
    """
    duration = len(wav) / sr
    segments: List[AudioSegment] = []

    for i, line in enumerate(lines):
        if not line.text.strip():
            continue

        start = line.start_s
        if start >= duration:
            continue

        end = lines[i + 1].start_s if i + 1 < len(lines) else duration

        seg_start = max(0, start - pad)
        seg_end = min(duration, end + pad)

        if seg_end - seg_start < min_seg:
            seg_end = min(duration, seg_start + min_seg)

        a = int(seg_start * sr)
        b = int(seg_end * sr)
        if b <= a:
            continue

        seg_wav = wav[a:b]
        segments.append(AudioSegment(wav=seg_wav, sr=sr, offset_s=seg_start, line=line))

    return segments


def create_aligner(model: str, device: str, dtype: str) -> Qwen3ForcedAligner:
    """Create a Qwen3ForcedAligner instance."""
    dtype_map = {"bfloat16": torch.bfloat16, "float16": torch.float16, "float32": torch.float32}
    return Qwen3ForcedAligner.from_pretrained(model, dtype=dtype_map[dtype], device_map=device)


def align_segments(
    aligner: Qwen3ForcedAligner,
    segments: List[AudioSegment],
    language: str,
    batch_size: int = 8,
) -> List[AlignedLine]:
    """
    Align audio segments using Qwen3ForcedAligner.

    Args:
        aligner: The forced aligner instance.
        segments: Audio segments to align.
        language: Language for alignment (e.g., "Chinese", "English").
        batch_size: Number of segments to process in each batch.

    Returns:
        List of AlignedLine with word-level timestamps.
    """
    aligned_lines: List[AlignedLine] = []

    for i in range(0, len(segments), batch_size):
        batch = segments[i : i + batch_size]

        results = aligner.align(
            audio=[(s.wav, s.sr) for s in batch],
            text=[s.line.text for s in batch],
            language=[language] * len(batch),
        )

        for seg, tokens in zip(batch, results):
            words = [
                AlignedWord(
                    text=token.text,
                    start_s=seg.offset_s + token.start_time,
                    end_s=seg.offset_s + token.end_time,
                )
                for token in tokens
            ]
            aligned_lines.append(AlignedLine(line=seg.line, words=words))

    return aligned_lines


def format_timestamp(seconds: float) -> str:
    """Convert seconds to mm:ss.xx format (centiseconds)."""
    total_cs = max(0, int(round(seconds * 100)))
    mm = total_cs // 6000
    ss = (total_cs % 6000) // 100
    xx = total_cs % 100
    return f"{mm:02d}:{ss:02d}.{xx:02d}"


def format_elrc_line(aligned: AlignedLine, include_end: bool = True) -> str:
    """Format an aligned line as ELRC with word-level timestamps."""
    line_ts = format_timestamp(aligned.line.start_s)
    if include_end and aligned.words:
        word_parts = [f"<{format_timestamp(w.start_s)}> {w.text} <{format_timestamp(w.end_s)}>" for w in aligned.words[:-1]]
        last = aligned.words[-1]
        word_parts.append(f"<{format_timestamp(last.start_s)}> {last.text}")
        return f"[{line_ts}] " + "   ".join(word_parts)
    else:
        word_parts = [f"<{format_timestamp(w.start_s)}> {w.text}" for w in aligned.words]
        return f"[{line_ts}] " + " ".join(word_parts)


def validate_alignment(aligned_lines: List[AlignedLine], all_lines: List[LrcLine]) -> None:
    """Print warnings for suspicious alignment results."""
    sorted_lines = sorted(all_lines, key=lambda x: (x.start_s, x.idx))
    idx_to_next_start: Dict[int, float] = {}
    for i, line in enumerate(sorted_lines):
        if i + 1 < len(sorted_lines):
            idx_to_next_start[line.idx] = sorted_lines[i + 1].start_s

    for aligned in aligned_lines:
        line = aligned.line
        words = aligned.words
        line_ts = format_timestamp(line.start_s)

        if not words:
            continue

        for w in words:
            if w.end_s <= w.start_s:
                print(f"Warning: Line {line.idx} [{line_ts}] - word '{w.text}' has zero/negative duration ({w.start_s:.2f}s -> {w.end_s:.2f}s)")

        if words[0].start_s < line.start_s:
            print(f"Warning: Line {line.idx} [{line_ts}] - first word '{words[0].text}' starts before line timestamp ({words[0].start_s:.2f}s < {line.start_s:.2f}s)")

        if line.idx in idx_to_next_start:
            next_start = idx_to_next_start[line.idx]
            if words[-1].end_s > next_start:
                print(f"Warning: Line {line.idx} [{line_ts}] - last word '{words[-1].text}' ends after next line starts ({words[-1].end_s:.2f}s > {next_start:.2f}s)")


def write_elrc(
    output_path: Path,
    metadata: Dict[str, str],
    aligned_lines: List[AlignedLine],
    all_lines: List[LrcLine],
    include_end: bool = True,
) -> None:
    """
    Write ELRC output file.

    Args:
        output_path: Path to write the ELRC file.
        metadata: Metadata dict from LRC parsing.
        aligned_lines: Aligned lines with word timestamps.
        all_lines: All original LRC lines (for merging empty ones).
        include_end: Whether to include end timestamps for each word.
    """
    aligned_idx = {al.line.idx for al in aligned_lines}
    idx_to_aligned = {al.line.idx: al for al in aligned_lines}

    with open(output_path, "w", encoding="utf-8") as f:
        for key, value in metadata.items():
            f.write(f"[{key}:{value}]\n")

        for line in sorted(all_lines, key=lambda x: (x.start_s, x.idx)):
            if line.idx in aligned_idx:
                f.write(format_elrc_line(idx_to_aligned[line.idx], include_end) + "\n")
            else:
                f.write(f"[{format_timestamp(line.start_s)}]\n")


def main() -> None:
    parser = argparse.ArgumentParser(
        description="LRC to ELRC converter using Qwen3ForcedAligner"
    )
    parser.add_argument(
        "--lrc",
        type=Path,
        required=True,
        help="Path to .lrc file",
    )
    parser.add_argument(
        "--audio",
        type=Path,
        required=True,
        help="Path to audio file",
    )
    parser.add_argument(
        "--pad",
        type=float,
        default=0.2,
        help="Padding in seconds around each segment",
    )
    parser.add_argument(
        "--min-seg",
        type=float,
        default=0.5,
        help="Minimum segment duration in seconds",
    )
    parser.add_argument(
        "--model",
        type=str,
        default="Qwen/Qwen3-ForcedAligner-0.6B",
        help="Path or HuggingFace model ID for aligner",
    )
    parser.add_argument(
        "--device",
        type=str,
        default="cuda:0",
        help="Device for inference",
    )
    parser.add_argument(
        "--dtype",
        type=str,
        default="bfloat16",
        choices=["bfloat16", "float16", "float32"],
        help="Data type for inference",
    )
    parser.add_argument(
        "--language",
        type=str,
        required=True,
        help="Language: Chinese, English, etc.",
    )
    parser.add_argument(
        "--batch-size",
        type=int,
        default=8,
        help="Batch size for alignment",
    )
    parser.add_argument(
        "--debug",
        action="store_true",
        help="Enable debug output",
    )
    parser.add_argument(
        "--no-end-timestamps",
        action="store_true",
        help="Omit word end timestamps (simpler ELRC format)",
    )
    parser.add_argument(
        "--out",
        type=Path,
        default=None,
        help="Output ELRC file path (default: same as --lrc with .elrc extension)",
    )
    parser.add_argument(
        "--separate-vocals",
        action="store_true",
        help="Use Demucs to isolate vocals before alignment (improves accuracy for music)",
    )
    args = parser.parse_args()

    if not args.lrc.exists():
        parser.error(f"File not found: {args.lrc}")
    if not args.audio.exists():
        parser.error(f"File not found: {args.audio}")

    metadata, lines = parse_lrc(args.lrc)

    if args.debug:
        # Print metadata
        if metadata:
            print("=== Metadata ===")
            for tag, value in metadata.items():
                print(f"  {tag}: {value}")
            print()

        # Print parsed lines
        print(f"=== Lyrics ({len(lines)} lines) ===")
        for line in lines:
            print(f"[{line.start_s:07.3f}] {line.text!r}")

    # Load audio (stereo if separating vocals, mono otherwise)
    wav, sr = load_audio(args.audio, mono=not args.separate_vocals)
    duration = len(wav) / sr if wav.ndim == 1 else wav.shape[0] / sr
    print(f"\nAudio loaded: {duration:.2f}s @ {sr}Hz")

    if args.separate_vocals:
        print("Separating vocals with Demucs (this may take a while)...")
        wav, sr = separate_vocals_demucs(wav, sr, device=args.device)
        duration = len(wav) / sr
        print(f"Vocal stem ready: {duration:.2f}s @ {sr}Hz")

    # Slice audio into segments
    segments = slice_audio(wav, sr, lines, pad=args.pad, min_seg=args.min_seg)
    print(f"Created {len(segments)} audio segments")

    if args.debug:
        print("\n=== Segment Durations ===")
        for seg in segments:
            seg_duration = len(seg.wav) / seg.sr
            print(f"[{seg.offset_s:07.3f}] {seg_duration:5.2f}s  {seg.line.text!r}")

    print(f"\nLoading aligner: {args.model}")
    aligner = create_aligner(args.model, args.device, args.dtype)

    print(f"Aligning {len(segments)} segments (batch_size={args.batch_size})...")
    aligned = align_segments(aligner, segments, args.language, args.batch_size)
    print(f"Aligned {len(aligned)} lines")

    validate_alignment(aligned, lines)

    output_path = args.out or args.lrc.with_suffix(".elrc")
    write_elrc(output_path, metadata, aligned, lines, include_end=not args.no_end_timestamps)
    print(f"ELRC written to {output_path}")


if __name__ == "__main__":
    main()
