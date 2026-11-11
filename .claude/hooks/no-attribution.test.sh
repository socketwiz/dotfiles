#!/usr/bin/env bash
# Tests for no-attribution.sh.
# Usage: bash ~/.claude/hooks/no-attribution.test.sh

set -u

HOOK=$(cd -- "$(dirname -- "$0")" && pwd)/no-attribution.sh
work=$(mktemp -d "${TMPDIR:-/tmp}/no-attribution-test.XXXXXX")
trap 'command rm -rf -- "$work"' EXIT

pass=0
fail=0

# expect deny|allow NAME COMMAND
expect() {
  local want=$1 name=$2 cmd=$3 out got
  out=$(jq -n --arg c "$cmd" --arg d "$work" '{tool_name: "Bash", cwd: $d, tool_input: {command: $c}}' | bash "$HOOK")
  if grep -q '"permissionDecision": "deny"' <<<"$out"; then got=deny; else got=allow; fi
  if [ "$got" = "$want" ]; then
    pass=$((pass + 1))
  else
    fail=$((fail + 1))
    echo "FAIL: $name (wanted $want, got $got)"
  fi
}

printf 'Fix it\n\n* Change\n\nCo-Authored-By: Claude Opus 5 <noreply@anthropic.com>\n' >"$work/bad.txt"
printf 'Fix it\n\n* Change\n' >"$work/good.txt"
printf 'Fix it\n\nCo-authored-by: Ricky Nelson <ricky@example.com>\n' >"$work/human.txt"

expect deny "trailer in -m" 'git commit -m "Fix it

Co-Authored-By: Claude <noreply@anthropic.com>"'
expect deny "trailer in a heredoc" "git commit -F - <<'EOF'
Fix it

Co-authored-by: Paperclip <noreply@paperclip.ing>
EOF"
expect deny "generated-with line in a PR body" 'tea pr create --description "Body

Generated with [Claude Code](https://claude.com/claude-code)"'
expect deny "trailer in a -F file" "git commit -F bad.txt"
expect deny "trailer in a --file= file" "git commit --file=$work/bad.txt"
expect deny "trailer in a quoted -F file" "git commit -F 'bad.txt'"
expect allow "clean -F file" "git commit -F good.txt"
expect allow "a human co-author" "git commit -F human.txt"
expect allow "missing -F file" "git commit -F nowhere.txt"
expect allow "clean commit" 'git commit -m "Fix it"'
expect allow "searching history" "git log -i --grep=co-authored-by"

echo "$pass passed, $fail failed"
[ "$fail" -eq 0 ]
