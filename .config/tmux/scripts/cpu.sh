#!/usr/bin/env bash
# CPU activity for tmux status bar (Linux & macOS)

CACHE="/tmp/tmux-cpu-$$-cache"
CACHE_GLOB="/tmp/tmux-cpu-*-cache"

# Clean stale cache files (keep only the most recent)
cleanup_cache() {
    for f in $CACHE_GLOB; do
        [[ "$f" != "$CACHE" ]] && rm -f "$f" 2>/dev/null
    done
}

get_cpu_linux() {
    read -r _ user nice system idle iowait irq softirq steal _ < /proc/stat

    local total=$((user + nice + system + idle + iowait + irq + softirq + steal))
    local busy=$((total - idle - iowait))

    # Find any existing cache file
    local cache_file=""
    for f in $CACHE_GLOB; do
        [[ -f "$f" ]] && cache_file="$f" && break
    done

    if [[ -n "$cache_file" ]]; then
        read -r prev_total prev_busy < "$cache_file"
        local dt=$((total - prev_total))
        local db=$((busy - prev_busy))
        if ((dt > 0)); then
            echo $((db * 100 / dt))
        else
            echo 0
        fi
        # Update the cache (reuse same file or write new)
        echo "$total $busy" > "$cache_file"
    else
        echo "$total $busy" > "$CACHE"
        echo 0
    fi
    cleanup_cache
}

get_cpu_darwin() {
    top -l 1 -n 0 2>/dev/null | awk '/CPU usage/ {
        gsub(/%/, "")
        printf "%.0f", $3 + $5
    }'
}

case "$(uname)" in
    Linux)  pct=$(get_cpu_linux) ;;
    Darwin) pct=$(get_cpu_darwin) ;;
    *)      pct=0 ;;
esac

# Color based on load: muted < 30%, cyan < 70%, pink >= 70%
if ((pct >= 70)); then
    color="#f7768e"
elif ((pct >= 30)); then
    color="#7dcfff"
else
    color="#565f89"
fi

printf "#[fg=%s]CPU %s%% " "$color" "$pct"
