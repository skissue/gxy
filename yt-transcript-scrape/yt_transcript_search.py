from __future__ import annotations

import argparse
import json
import os
import re
import sys
import time
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Iterable

from youtube_transcript_api import YouTubeTranscriptApi
from youtube_transcript_api._errors import (
    InvalidVideoId,
    NoTranscriptFound,
    RequestBlocked,
    TranscriptsDisabled,
    VideoUnavailable,
)

APP_NAME = "yt-transcript-scrape"
TTY_HIGHLIGHT_START = "\x1b[1;31m"
TTY_HIGHLIGHT_END = "\x1b[0m"
DEFAULT_DOWNLOAD_INTERVAL = 0.5


class DownloadRateLimiter:
    def __init__(self, interval_seconds: float) -> None:
        self.interval_seconds = max(interval_seconds, 0.0)
        self._last_fetch_started_at: float | None = None

    def wait(self) -> None:
        if self._last_fetch_started_at is None or self.interval_seconds == 0:
            self._last_fetch_started_at = time.monotonic()
            return

        elapsed = time.monotonic() - self._last_fetch_started_at
        remaining = self.interval_seconds - elapsed
        if remaining > 0:
            time.sleep(remaining)
        self._last_fetch_started_at = time.monotonic()


@dataclass
class CachedTranscript:
    video_id: str
    language_code: str | None
    language: str | None
    is_generated: bool | None
    fetched_at: str | None
    segments: list[dict[str, object]]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Search a regex across YouTube transcripts for video IDs from stdin."
    )
    parser.add_argument("pattern", help="Python regex to search for")
    parser.add_argument(
        "--ignore-case",
        action="store_true",
        help="Use case-insensitive regex matching",
    )
    parser.add_argument(
        "--refresh",
        action="store_true",
        help="Refetch transcripts instead of using cached copies",
    )
    parser.add_argument(
        "--languages",
        help="Comma-separated language codes to prefer, for example en,de",
    )
    parser.add_argument(
        "--download-interval",
        type=float,
        default=DEFAULT_DOWNLOAD_INTERVAL,
        help=(
            "Minimum seconds between uncached transcript downloads "
            f"(default: {DEFAULT_DOWNLOAD_INTERVAL})"
        ),
    )
    return parser.parse_args()


def get_cache_root() -> Path:
    base = os.environ.get("XDG_CACHE_HOME")
    if base:
        return Path(base).expanduser() / APP_NAME
    return Path.home() / ".cache" / APP_NAME


def language_key(languages: list[str] | None) -> str:
    if not languages:
        return "default"
    return ",".join(languages)


def cache_path(video_id: str, languages: list[str] | None) -> Path:
    key = language_key(languages).replace("/", "_")
    return get_cache_root() / "transcripts" / key / f"{video_id}.json"


def format_timestamp(seconds: float) -> str:
    total_millis = round(seconds * 1000)
    hours, remainder = divmod(total_millis, 3_600_000)
    minutes, remainder = divmod(remainder, 60_000)
    secs, millis = divmod(remainder, 1000)
    if hours:
        return f"{hours:02d}:{minutes:02d}:{secs:02d}.{millis:03d}"
    return f"{minutes:02d}:{secs:02d}.{millis:03d}"


def warn(message: str) -> None:
    print(f"warning: {message}", file=sys.stderr)


def read_video_ids(lines: Iterable[str]) -> list[str]:
    seen: dict[str, None] = {}
    for raw_line in lines:
        video_id = raw_line.strip()
        if not video_id:
            continue
        seen.setdefault(video_id, None)
    return list(seen)


def compile_pattern(pattern: str, ignore_case: bool) -> re.Pattern[str]:
    flags = re.IGNORECASE if ignore_case else 0
    try:
        compiled = re.compile(pattern, flags)
    except re.error as exc:
        raise SystemExit(f"invalid regex: {exc}") from exc

    if compiled.search("") is not None:
        raise SystemExit("regex must not match the empty string")

    return compiled


def highlight_matches(text: str, pattern: re.Pattern[str], use_color: bool) -> str:
    if use_color:
        start_marker = TTY_HIGHLIGHT_START
        end_marker = TTY_HIGHLIGHT_END
        return pattern.sub(
            lambda match: f"{start_marker}{match.group(0)}{end_marker}", text
        )
    return text


def normalize_segment_text(text: str) -> str:
    return " ".join(text.split())


def load_cached_transcript(path: Path) -> CachedTranscript | None:
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except FileNotFoundError:
        return None
    except json.JSONDecodeError:
        warn(f"ignoring corrupt cache file {path}")
        return None

    if not isinstance(payload, dict) or not isinstance(payload.get("segments"), list):
        warn(f"ignoring malformed cache file {path}")
        return None

    return CachedTranscript(
        video_id=str(payload.get("video_id", "")),
        language_code=payload.get("language_code"),
        language=payload.get("language"),
        is_generated=payload.get("is_generated"),
        fetched_at=payload.get("fetched_at"),
        segments=payload["segments"],
    )


def save_transcript(path: Path, transcript: CachedTranscript) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        json.dumps(
            {
                "video_id": transcript.video_id,
                "language_code": transcript.language_code,
                "language": transcript.language,
                "is_generated": transcript.is_generated,
                "fetched_at": transcript.fetched_at,
                "segments": transcript.segments,
            },
            ensure_ascii=True,
            indent=2,
        )
        + "\n",
        encoding="utf-8",
    )


def fetch_transcript(
    video_id: str,
    languages: list[str] | None,
    rate_limiter: DownloadRateLimiter,
) -> CachedTranscript | None:
    api = YouTubeTranscriptApi()
    try:
        rate_limiter.wait()
        fetch_kwargs: dict[str, object] = {"video_id": video_id}
        if languages is not None:
            fetch_kwargs["languages"] = languages
        transcript = api.fetch(**fetch_kwargs)
    except (NoTranscriptFound, TranscriptsDisabled):
        warn(f"{video_id}: missing transcript")
        return None
    except InvalidVideoId as exc:
        warn(f"{video_id}: invalid video id ({exc})")
        return None
    except VideoUnavailable:
        warn(f"{video_id}: video unavailable")
        return None
    except RequestBlocked as exc:
        warn(f"{video_id}: transcript request blocked ({exc})")
        return None
    except Exception as exc:
        warn(f"{video_id}: transcript fetch failed ({exc})")
        return None

    return CachedTranscript(
        video_id=transcript.video_id,
        language_code=transcript.language_code,
        language=transcript.language,
        is_generated=transcript.is_generated,
        fetched_at=datetime.now(timezone.utc).isoformat(),
        segments=transcript.to_raw_data(),
    )


def get_transcript(
    video_id: str,
    languages: list[str] | None,
    refresh: bool,
    rate_limiter: DownloadRateLimiter,
) -> CachedTranscript | None:
    path = cache_path(video_id, languages)

    if not refresh:
        cached = load_cached_transcript(path)
        if cached is not None:
            return cached

    transcript = fetch_transcript(video_id, languages, rate_limiter)
    if transcript is None:
        return None

    save_transcript(path, transcript)
    return transcript


def search_segments(
    video_id: str,
    segments: list[dict[str, object]],
    pattern: re.Pattern[str],
    use_color: bool,
) -> int:
    matches = 0
    for segment in segments:
        text = segment.get("text")
        start = segment.get("start")
        if not isinstance(text, str) or not isinstance(start, (int, float)):
            continue
        if pattern.search(text) is None:
            continue
        highlighted = normalize_segment_text(
            highlight_matches(text, pattern, use_color)
        )
        print(f"{video_id}:{format_timestamp(float(start))}:{highlighted}")
        matches += 1
    return matches


def parse_languages(raw: str | None) -> list[str] | None:
    if raw is None:
        return None
    languages = [item.strip() for item in raw.split(",") if item.strip()]
    return languages or None


def main() -> int:
    args = parse_args()
    if args.download_interval < 0:
        raise SystemExit("--download-interval must be >= 0")

    languages = parse_languages(args.languages)
    pattern = compile_pattern(args.pattern, args.ignore_case)
    use_color = sys.stdout.isatty()
    total_matches = 0
    rate_limiter = DownloadRateLimiter(args.download_interval)

    for video_id in read_video_ids(sys.stdin):
        transcript = get_transcript(
            video_id,
            languages,
            args.refresh,
            rate_limiter,
        )
        if transcript is None:
            continue
        total_matches += search_segments(
            video_id=video_id,
            segments=transcript.segments,
            pattern=pattern,
            use_color=use_color,
        )

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
