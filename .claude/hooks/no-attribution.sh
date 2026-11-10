#!/usr/bin/env bash
# PreToolUse hook (Bash): deny a command that would write an AI attribution
# line, such as a Co-Authored-By trailer naming Claude, Anthropic or
# Paperclip, or a "Generated with Claude" line, into a commit, tag or PR.
# Enforces the "No attribution" rule in ~/.claude/CLAUDE.md.
#
# Scans the command text, plus any message file given to -F / --file (a
# relative path resolves against the call's cwd). Fails closed without jq.

set -u

command -v jq >/dev/null 2>&1 || {
  echo "no-attribution hook: jq not found, so the attribution rule could not be checked" >&2
  exit 2
}

input=$(cat)
cmd=$(jq -r '.tool_input.command // ""' <<<"$input")
cwd=$(jq -r '.cwd // ""' <<<"$input")

PATTERN='co-authored-by:.*(claude|anthropic|paperclip)|generated with \[?claude'

# Paths passed as -F PATH, -FPATH, --file PATH or --file=PATH, quotes stripped.
message_files() {
  grep -oP '(?:^|\s)(?:-F\s*|--file[=\s]+)\K("[^"]+"|'"'"'[^'"'"']+'"'"'|[^\s;|&)]+)' <<<"$cmd" |
    sed -e 's/^["'"'"']//' -e 's/["'"'"']$//'
}

file_text() {
  local path=$1
  [ "$path" = "-" ] && return
  case "$path" in
    /*) ;;
    "~"/*) path="$HOME/${path#\~/}" ;;
    *) path="$cwd/$path" ;;
  esac
  [ -f "$path" ] && cat -- "$path"
}

text=$(
  printf '%s\n' "$cmd"
  message_files | while IFS= read -r f; do file_text "$f"; done
)

if grep -qiE "$PATTERN" <<<"$text"; then
  jq -n '{
    hookSpecificOutput: {
      hookEventName: "PreToolUse",
      permissionDecision: "deny",
      permissionDecisionReason: "This command adds an AI attribution line (a Co-Authored-By trailer naming Claude, Anthropic or Paperclip, or \"Generated with Claude\"). The No attribution rule in ~/.claude/CLAUDE.md forbids them in commits, tags and PRs. Remove the line and retry."
    }
  }'
fi
exit 0
