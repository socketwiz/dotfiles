#!/bin/bash
input=$(cat)

MODEL=$(echo "$input" | jq -r '.model.display_name')
CONTEXT_SIZE=$(echo "$input" | jq -r '.context_window.context_window_size')
USAGE=$(echo "$input" | jq '.context_window.current_usage')

format_tokens() {
    local num=$1
    if [ "$num" -ge 1000000 ]; then
        printf "%.1fM" "$(echo "scale=1; $num/1000000" | bc)"
    elif [ "$num" -ge 1000 ]; then
        printf "%.0fk" "$(echo "scale=0; $num/1000" | bc)"
    else
        echo "$num"
    fi
}

if [ "$USAGE" != "null" ] && [ "$CONTEXT_SIZE" != "null" ]; then
    CURRENT=$(echo "$USAGE" | jq '.input_tokens + .cache_creation_input_tokens + .cache_read_input_tokens + .output_tokens')
    LEFT=$((CONTEXT_SIZE - CURRENT))
    PCT=$((CURRENT * 100 / CONTEXT_SIZE))
    LEFT_FMT=$(format_tokens $LEFT)
    TOTAL_FMT=$(format_tokens $CONTEXT_SIZE)
    echo "[$MODEL] Context: ${LEFT_FMT}/${TOTAL_FMT} remaining (${PCT}% used)"
else
    echo "[$MODEL] Ready"
fi
