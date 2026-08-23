#!/usr/bin/env bash
# PreToolUse hook: deny Write/Edit/NotebookEdit calls that INTRODUCE em dashes
# (U+2014) or en dashes (U+2013).
#
# Dashes are matched via \x{...} escapes rather than literal characters so
# this script never trips its own rule when it is edited.
#
# Every call is scored as "dashes in the new text" vs "dashes in the text it
# replaces", so a change that merely preserves dashes already on disk passes,
# per the exception in ~/.claude/CLAUDE.md.
#
# The check fails CLOSED: if the scan cannot run (no jq, no grep -P, a locale
# where the escapes will not compile), the call is denied rather than waved
# through unchecked.

set -u

# grep -P's \x{...} escapes only compile in UTF-8 mode. Under a non-UTF-8
# locale grep aborts, every count comes back 0, and dashes sail through.
export LC_ALL=C.UTF-8

# Emit a PreToolUse deny decision. Needs jq; see block_without_jq for the
# path taken when jq itself is what is missing.
deny() {
  jq -n --arg r "$1" '{
    hookSpecificOutput: {
      hookEventName: "PreToolUse",
      permissionDecision: "deny",
      permissionDecisionReason: $r
    }
  }'
  exit 0
}

# Fallback deny for when jq is unavailable: exit 2 makes Claude Code block the
# call and feed stderr back to the model.
block_without_jq() {
  printf 'no-dashes hook: %s\n' "$1" >&2
  exit 2
}

command -v jq >/dev/null 2>&1 ||
  block_without_jq "jq not found, so the punctuation rule could not be checked"

# Positive control: build a real em dash at runtime (the source stays clean)
# and confirm the matcher finds it. Catches a grep built without -P as well as
# any locale in which the escapes fail to compile.
probe=$(printf '\u2014')
if [ "$(printf '%s' "$probe" | grep -oP '\x{2014}' 2>/dev/null | wc -l)" != "1" ]; then
  deny "The no-dashes hook failed its own self-test: grep -P is missing, or \x{...} escapes will not compile in this locale. The punctuation rule could not be checked, so the call was denied."
fi

input=$(cat)
tool=$(jq -r '.tool_name // ""' <<<"$input")

# Contents of a file, or empty when the path is absent or unreadable.
read_file() {
  [ -n "$1" ] && [ -f "$1" ] && cat -- "$1"
}

case "$tool" in
  Write)
    new=$(jq -r '.tool_input.content // ""' <<<"$input")
    # Baseline is the file being overwritten, so rewriting a document that
    # already quotes a dash does not count as introducing one.
    old=$(read_file "$(jq -r '.tool_input.file_path // ""' <<<"$input")")
    ;;
  Edit)
    new=$(jq -r '.tool_input.new_string // ""' <<<"$input")
    old=$(jq -r '.tool_input.old_string // ""' <<<"$input")
    ;;
  NotebookEdit)
    new=$(jq -r '.tool_input.new_source // ""' <<<"$input")
    # Baseline is the cell being replaced. An insert replaces nothing (its
    # cell_id names the cell it lands after), so it starts from zero.
    old=""
    if [ "$(jq -r '.tool_input.edit_mode // "replace"' <<<"$input")" = "replace" ]; then
      notebook=$(jq -r '.tool_input.notebook_path // ""' <<<"$input")
      cell=$(jq -r '.tool_input.cell_id // ""' <<<"$input")
      if [ -n "$cell" ] && [ -f "$notebook" ]; then
        # .source is a string in some notebooks and a list of lines in others.
        old=$(jq -r --arg id "$cell" \
          '[.cells[]? | select(.id == $id) | .source] | flatten | join("")' \
          "$notebook" 2>/dev/null) || old=""
      fi
    fi
    ;;
  *)
    exit 0
    ;;
esac

# Number of dashes in the argument, or a non-zero return when grep itself
# errored (exit 1 just means "no matches", which is a legitimate count of 0).
count_dashes() {
  local matches status
  matches=$(printf '%s' "$1" | grep -oP '\x{2014}|\x{2013}')
  status=$?
  if [ "$status" -gt 1 ]; then
    return 1
  fi
  if [ -z "$matches" ]; then
    printf '0'
    return 0
  fi
  printf '%s\n' "$matches" | wc -l
}

new_n=$(count_dashes "$new") ||
  deny "The no-dashes hook could not scan the new text, so the call was denied rather than allowed unchecked."
old_n=$(count_dashes "$old") ||
  deny "The no-dashes hook could not scan the existing text, so the call was denied rather than allowed unchecked."

if [ "$new_n" -gt "$old_n" ]; then
  deny 'This edit introduces an em dash or en dash, which is forbidden by the Punctuation rule in ~/.claude/CLAUDE.md. Rewrite using a comma, colon, semicolon, period, or parentheses, then retry. Use a plain hyphen (-) for compounds and ranges.'
fi

exit 0
