#!/usr/bin/env bash
# PreToolUse hook: deny Write/Edit calls that INTRODUCE em dashes (U+2014)
# or en dashes (U+2013).
#
# Dashes are matched via \x{...} escapes rather than literal characters so
# this script never trips its own rule when it is edited.
#
# For Edit, the new text is compared against the old text: an edit that
# merely preserves dashes already present in the file is allowed through.

set -u

input=$(cat)
tool=$(jq -r '.tool_name // ""' <<<"$input")

case "$tool" in
  Write)
    new=$(jq -r '.tool_input.content // ""' <<<"$input")
    old=""
    ;;
  Edit)
    new=$(jq -r '.tool_input.new_string // ""' <<<"$input")
    old=$(jq -r '.tool_input.old_string // ""' <<<"$input")
    ;;
  *)
    exit 0
    ;;
esac

count_dashes() {
  printf '%s' "$1" | grep -oP '\x{2014}|\x{2013}' | wc -l
}

new_n=$(count_dashes "$new")
old_n=$(count_dashes "$old")

if [ "$new_n" -gt "$old_n" ]; then
  reason='This edit introduces an em dash or en dash, which is forbidden by the Punctuation rule in ~/.claude/CLAUDE.md. Rewrite using a comma, colon, semicolon, period, or parentheses, then retry. Use a plain hyphen (-) for compounds and ranges.'
  jq -n --arg r "$reason" '{
    hookSpecificOutput: {
      hookEventName: "PreToolUse",
      permissionDecision: "deny",
      permissionDecisionReason: $r
    }
  }'
fi

exit 0
