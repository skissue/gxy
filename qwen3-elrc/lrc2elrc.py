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


@dataclass
class LrcLine:
    """A single LRC lyric line."""
    start_s: float
    text: str
    raw: str
    idx: int


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


def load_audio(filepath: Path) -> Tuple[np.ndarray, int]:
    """
    Load audio file using soundfile.

    Returns:
        Tuple of (waveform as mono float32 ndarray, sample rate)
    """
    if not filepath.exists():
        raise FileNotFoundError(f"Audio file not found: {filepath}")

    try:
        wav, sr = sf.read(filepath, dtype="float32")
    except sf.SoundFileError as e:
        raise RuntimeError(f"Unsupported audio format or corrupted file: {filepath}: {e}")

    if wav.ndim == 2:
        wav = wav.mean(axis=1)

    return np.asarray(wav, dtype=np.float32), int(sr)


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
    args = parser.parse_args()

    if not args.lrc.exists():
        parser.error(f"File not found: {args.lrc}")
    if not args.audio.exists():
        parser.error(f"File not found: {args.audio}")

    metadata, lines = parse_lrc(args.lrc)

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

    # Load audio
    wav, sr = load_audio(args.audio)
    duration = len(wav) / sr
    print(f"\nAudio loaded: {duration:.2f}s @ {sr}Hz")


if __name__ == "__main__":
    main()
