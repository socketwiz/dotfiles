#!/usr/bin/env bash
# Usage: ./mp4-to-gif.sh in.mp4 out.gif [fps] [colors] [maxw] [opt]
# Defaults: fps=9, colors=64, maxw=720, opt=safe
# opt ∈ {none|safe|aggressive}

set -euo pipefail

IN="${1:?input required}"
OUT="${2:?output required}"
FPS="${3:-9}"
COLORS="${4:-64}"
MAXW="${5:-1920}"
OPT="${6:-safe}"

# Choose ONE scale. Default: cap width at MAXW while preserving AR (safer size).
SCALE="scale=min(ceil(iw/2)*2\\,$MAXW):-2:flags=lanczos"
# For original dimensions (bigger files), use:
# SCALE="scale=ceil(iw/2)*2:ceil(ih/2)*2:flags=lanczos"

# mktemp on macOS needs manual extension
PALETTE="$(mktemp -t palette).png"
cleanup() { rm -f "$PALETTE"; }
trap cleanup EXIT

# 1) Generate palette (dedupe frames -> better palette allocation)
ffmpeg -y -i "$IN" \
  -vf "mpdecimate,fps=${FPS},${SCALE},palettegen=stats_mode=diff:max_colors=${COLORS}" \
  -hide_banner -loglevel error "$PALETTE"

# 2) Apply palette
# alpha_threshold=128 clamps semi-transparent pixels to fully opaque/transparent
# which makes downstream optimizers (and some viewers) happier.
ffmpeg -y -i "$IN" -i "$PALETTE" \
  -filter_complex "[0:v]mpdecimate,fps=${FPS},${SCALE}[x];[x][1:v]paletteuse=dither=sierra2_4a:diff_mode=rectangle:alpha_threshold=128" \
  -loop 0 -hide_banner -loglevel error "$OUT"

# 3) Optional post-optimization with gifsicle
if [[ "$OPT" != "none" ]] && command -v gifsicle >/dev/null 2>&1; then
  case "$OPT" in
    safe)
      # Safer optimization; preserves timing & loop count, fewer risky transforms.
      gifsicle -O2 --careful --same-loopcount --same-delay "$OUT" -o "$OUT"
      ;;
    aggressive)
      # Try aggressive; if it breaks, fall back to safe.
      TMP_OPT="$(mktemp -t gifopt).gif"
      if gifsicle -O3 --lossy=40 --careful --same-loopcount --same-delay "$OUT" -o "$TMP_OPT"; then
        mv "$TMP_OPT" "$OUT"
      else
        rm -f "$TMP_OPT"
        gifsicle -O2 --careful --same-loopcount --same-delay "$OUT" -o "$OUT"
      fi
      ;;
    *)
      echo "Unknown opt '$OPT' (use none|safe|aggressive). Proceeding without gifsicle." >&2
      ;;
  esac
fi

