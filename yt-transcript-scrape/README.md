# yt-transcript-scrape

Batch-search a regex across YouTube transcripts for video IDs read from stdin.

## Usage

```bash
uv run yt-transcript-search 'pattern' < video_ids.txt
```

Options:

- `--ignore-case` for case-insensitive regex matching
- `--refresh` to bypass the local transcript cache
- `--languages en,de` to prefer specific transcript languages
- `--download-interval 1.0` to wait between uncached transcript downloads

Transcripts are cached under `$XDG_CACHE_HOME/yt-transcript-scrape/` or `~/.cache/yt-transcript-scrape/` when `XDG_CACHE_HOME` is unset.
