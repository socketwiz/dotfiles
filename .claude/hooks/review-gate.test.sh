#!/usr/bin/env bash
# Tests for review-gate.sh. A stub replaces `claude`, so no real review runs.
# Usage: bash ~/.claude/hooks/review-gate.test.sh

set -u

HOOK=$(cd -- "$(dirname -- "$0")" && pwd)/review-gate.sh
work=$(mktemp -d "${TMPDIR:-/tmp}/review-gate-test.XXXXXX")
trap 'command rm -rf -- "$work"' EXIT

pass=0
fail=0

# Stub claude: records its cwd, args and scratch env, leaves a build artifact
# behind, then prints any events in $work/events and the verdict in
# $work/verdict.
cat >"$work/claude" <<'EOF'
#!/usr/bin/env bash
dir=$(dirname "$0")
echo "$PWD" >>"$dir/calls"
printf '%s\n' "$@" >"$dir/last-args"
printf '%s\n' "${CARGO_TARGET_DIR:-}" >"$dir/last-target"
printf '%s\n' "${TMPDIR:-}" >"$dir/last-tmpdir"
printf '%s\n' "${CLAUDE_CODE_PRINT_BG_WAIT_CEILING_MS-unset}" >"$dir/last-ceiling"
mkdir -p "$CARGO_TARGET_DIR" && echo built >"$CARGO_TARGET_DIR/artifact"
[ -e "$dir/stub-exit" ] && exit "$(cat "$dir/stub-exit")"
[ -e "$dir/events" ] && cat "$dir/events"
cat "$dir/verdict"
EOF
chmod +x "$work/claude"
export REVIEW_GATE_CLAUDE=$work/claude

# Stub npm: records its cwd and args, then fails if $work/npm-exit says so
mkdir -p "$work/bin"
cat >"$work/bin/npm" <<'EOF2'
#!/usr/bin/env bash
dir=$(dirname "$0")/..
echo "$PWD $*" >>"$dir/npm-calls"
[ -e "$dir/npm-exit" ] && exit "$(cat "$dir/npm-exit")"
exit 0
EOF2
chmod +x "$work/bin/npm"
npm_calls() { [ -e "$work/npm-calls" ] && wc -l <"$work/npm-calls" || echo 0; }

verdict() { # critical important [suggestions]
  jq -n --argjson c "$1" --argjson i "$2" --argjson s "${3:-0}" \
    '{type: "result", structured_output: {critical: $c, important: $i, suggestions: $s, report: "## Report c=\($c) i=\($i) s=\($s)"}}' >"$work/verdict"
}

calls() { [ -e "$work/calls" ] && wc -l <"$work/calls" || echo 0; }

# Run the hook for a Bash command in $repo. Prints "allow" or "deny: <reason>".
run() {
  local out
  out=$(jq -n --arg c "$1" --arg d "$repo" \
    '{tool_name: "Bash", tool_input: {command: $c}, cwd: $d}' |
    TMPDIR=$work REVIEW_GATE_DIR=$work PATH="$work/bin:$PATH" bash "$HOOK")
  if [ -z "$out" ]; then
    echo allow
  else
    echo "deny: $(jq -r '.hookSpecificOutput.permissionDecisionReason' <<<"$out")"
  fi
}

check() { # name expected-prefix actual
  if [[ "$3" == "$2"* ]]; then
    pass=$((pass + 1))
  else
    fail=$((fail + 1))
    printf 'FAIL %s\n  expected: %s...\n  got:      %s\n' "$1" "$2" "${3:0:300}"
  fi
}

new_repo() {
  repo=$work/repo-$1
  git init -q -b main "$repo"
  git -C "$repo" -c user.name=t -c user.email=t@example.invalid commit -q --allow-empty -m init
  git init -q --bare "$work/origin-$1.git"
  git -C "$repo" remote add origin "$work/origin-$1.git"
  git -C "$repo" push -q origin main
  git -C "$repo" checkout -q -b "$2"
  command rm -f -- "$work/calls"
}

commit() {
  echo "$RANDOM" >>"$repo/f"
  git -C "$repo" add f
  git -C "$repo" -c user.name=t -c user.email=t@example.invalid commit -q -m change
}

# --- non-ticket branches and non-push commands pass untouched ---
new_repo a feature-x
commit
verdict 1 0
check "non-ticket branch" allow "$(run 'git push -u origin feature-x')"
git -C "$repo" checkout -q -b 12-thing
check "commit mentioning push" allow "$(run "git commit -m 'avoid push to main'")"
check "git log" allow "$(run 'git log --oneline')"
check "no review yet" 0 "$(calls)"

# --- branch with no commits beyond base needs no review ---
new_repo b 7-empty
check "nothing to review" allow "$(run 'git push -u origin 7-empty')"
check "empty branch skipped review" 0 "$(calls)"

# --- compound and redirected pushes are denied ---
new_repo c 3-fix
commit
check "chained push" "deny: A push involving a ticket branch" "$(run 'git commit -am x && git push')"
check "delete chained with push" "deny: A push involving a ticket branch" "$(run 'git push origin --delete x && git push')"
check "git -C push" "deny: A push involving a ticket branch" "$(run 'git -C /elsewhere push')"
check "multi-line push" "deny: A push involving a ticket branch" "$(run $'git status\ngit push')"
check "remote delete" allow "$(run 'git push origin --delete 3-old')"
check "colon delete" allow "$(run 'git push origin :3-old')"
check "no review for denied/deletes" 0 "$(calls)"

# --- full fail -> fix -> pass cycle ---
verdict 1 2
r=$(run 'git push -u origin 3-fix')
check "round 1 fails" "deny: Review round 1 of 3 found 1 Critical and 2 Important" "$r"
check "Critical count recorded" 1 "$(cat "$(git -C "$repo" rev-parse --absolute-git-dir)/review-gate/3-fix/last-critical")"
check "report included" "yes" "$([[ "$r" == *"## Report c=1 i=2"* ]] && echo yes)"
check "ran once" 1 "$(calls)"
check "ran in a worktree" yes "$([ "$(tail -1 "$work/calls")" != "$repo" ] && echo yes)"
check "hooks disabled in nested run" yes "$(command grep -q disableAllHooks "$work/last-args" && echo yes)"
check "worktree cleaned up" 1 "$(git -C "$repo" worktree list | wc -l)"
target=$(cat "$work/last-target")
check "review gets its own cargo target" "$work/review-gate-scratch." "$target"
check "review TMPDIR is the scratch dir" "${target%/target}" "$(cat "$work/last-tmpdir")"
check "scratch dir removed after review" gone "$([ -e "${target%/target}" ] || echo gone)"
check "review waits for every agent" 0 "$(cat "$work/last-ceiling")"
check "one test run per review" yes "$(command grep -q 'Run the test suite at most once' "$work/last-args" && echo yes)"
check "decisions and wording graded as Suggestions" yes "$(command grep -q 'Decision needed:' "$work/last-args" && command grep -q 'wording of comments and documentation' "$work/last-args" && echo yes)"
check "same HEAD is not re-reviewed" "deny: HEAD" "$(run 'git push')"
check "still one run" 1 "$(calls)"
commit
verdict 0 0
check "round 2 passes" allow "$(run 'git push')"
check "approved HEAD pushes freely" allow "$(run 'git push')"
check "two runs total" 2 "$(calls)"
state=$(git -C "$repo" rev-parse --absolute-git-dir)/review-gate/3-fix
check "round reports kept" yes "$([ -s "$state/round-1.md" ] && [ -s "$state/round-2.md" ] && echo yes)"

# --- refspecs: only the checked-out ticket branch, to its own name ---
new_repo g 20-mine
commit
git -C "$repo" branch 21-other main
git -C "$repo" branch feature-y main
check "other ticket branch from ticket branch" "deny: review-gate: on ticket branch '20-mine', this push sends '21-other'" "$(run 'git push origin 21-other')"
check "non-ticket branch from ticket branch" "deny: review-gate: on ticket branch '20-mine', this push sends 'feature-y'" "$(run 'git push origin feature-y')"
check "HEAD to another name" "deny: review-gate: on ticket branch '20-mine', this push sends 'HEAD' to '21-other'" "$(run 'git push origin HEAD:21-other')"
check "HEAD to refs/heads other name" "deny: review-gate: on ticket branch" "$(run 'git push origin HEAD:refs/heads/21-other')"
check "second refspec sneaks in" "deny: review-gate: on ticket branch" "$(run 'git push origin 20-mine 21-other')"
check "--all" "deny: review-gate: '--all'" "$(run 'git push --all origin')"
check "--mirror" "deny: review-gate: '--mirror'" "$(run 'git push --mirror origin')"
check "wildcard refspec" "deny: review-gate: 'refs/heads/*" "$(run 'git push origin refs/heads/*:refs/heads/*')"
check "force flag" "deny: review-gate: force" "$(run 'git push -f origin 20-mine')"
check "clustered force" "deny: review-gate: force" "$(run 'git push -uf origin 20-mine')"
check "force-with-lease" "deny: review-gate: force" "$(run 'git push --force-with-lease origin 20-mine')"
check "plus refspec" "deny: review-gate: force" "$(run 'git push origin +20-mine')"
git -C "$repo" config push.default matching
check "push.default=matching" "deny: review-gate: 'push.default=matching'" "$(run 'git push')"
git -C "$repo" config --unset push.default
check "dry run" allow "$(run 'git push -n origin 21-other')"
check "no review for refused refspecs" 0 "$(calls)"
verdict 0 0
check "explicit own branch" allow "$(run 'git push -u origin 20-mine')"
check "HEAD to own name" allow "$(run 'git push origin HEAD')"
check "HEAD:own name" allow "$(run 'git push origin HEAD:refs/heads/20-mine')"
check "one review for the allowed forms" 1 "$(calls)"

# --- pushing a ticket branch from a non-ticket branch ---
new_repo h main-work
commit
git -C "$repo" branch 30-sneak
verdict 1 0
check "ticket branch from non-ticket branch" "deny: review-gate: this push sends to ticket branch(es) 30-sneak from 'main-work'" "$(run 'git push origin 30-sneak')"
check "non-ticket src to ticket dst" "deny: review-gate: this push sends to ticket branch(es) 31-new" "$(run 'git push origin main-work:31-new')"
check "detached HEAD to ticket" "deny: review-gate: this push sends to ticket branch(es) 30-sneak from 'detached HEAD'" "$(git -C "$repo" checkout -q --detach && run 'git push origin HEAD:30-sneak')"
git -C "$repo" checkout -q main-work
check "--all with local ticket branch" "deny: review-gate: '--all'" "$(run 'git push --all origin')"
check "quoted ticket refspec" "deny: A push involving a ticket branch" "$(run "git push origin '30-sneak'")"
check "chained ticket push from non-ticket branch" "deny: A push involving a ticket branch" "$(run 'git fetch && git push origin 30-sneak')"
check "plain non-ticket push" allow "$(run 'git push -u origin main-work')"
check "non-ticket refspec" allow "$(run 'git push origin main-work:feature-z')"
check "chained non-ticket push" allow "$(run 'git commit -am x && git push')"
git -C "$repo" config push.default upstream
git -C "$repo" config branch.main-work.merge refs/heads/32-up
check "upstream maps to ticket branch" "deny: review-gate: this push sends to ticket branch(es) 32-up" "$(run 'git push')"
check "still no reviews" 0 "$(calls)"

# --- round cap ---
new_repo d 9-cap
verdict 0 1
for n in 1 2; do
  commit
  check "cap round $n" "deny: Review round $n of 3" "$(run 'git push')"
done
commit
r=$(run 'git push')
check "cap round 3" "deny: Review round 3 of 3 (the cap)" "$r"
check "cap asks for a cleanup issue" yes "$([[ "$r" == *"file one cleanup issue"* ]] && echo yes)"
state=$(git -C "$repo" rev-parse --absolute-git-dir)/review-gate/9-cap
check "cap records the last round's Critical count" 0 "$(cat "$state/last-critical")"
check "cap records the reviewed HEAD" "$(git -C "$repo" rev-parse HEAD)" "$(cat "$state/last-sha")"
check "push after cap allowed" allow "$(run 'git push')"
commit
check "later commits pass once capped" allow "$(run 'git push')"
check "no 4th round" 3 "$(calls)"

# --- REVIEW_GATE_MAX_ROUNDS moves the cap ---
new_repo l 10-cap2
verdict 0 1
commit
check "2-round cap, round 1" "deny: Review round 1 of 2 found" "$(REVIEW_GATE_MAX_ROUNDS=2 run 'git push')"
commit
check "2-round cap, round 2 is the cap" "deny: Review round 2 of 2 (the cap)" "$(REVIEW_GATE_MAX_ROUNDS=2 run 'git push')"
check "2-round cap, push after cap allowed" allow "$(REVIEW_GATE_MAX_ROUNDS=2 run 'git push')"
check "2-round cap, two reviews" 2 "$(calls)"

# --- a pass that leaves Suggestions asks for a cleanup issue once ---
new_repo i 40-tidy
commit
verdict 0 0 2
r=$(run 'git push')
check "pass with suggestions denied once" "deny: Review round 1 of 3 approved HEAD" "$r"
check "early round may apply them" yes "$([[ "$r" == *"a new commit starts round 2 of 3"* ]] && echo yes)"
check "suggestions report included" yes "$([[ "$r" == *"## Report c=0 i=0 s=2"* ]] && echo yes)"
check "approved HEAD then pushes" allow "$(run 'git push')"
check "one review for suggestions" 1 "$(calls)"
new_repo j 41-last
verdict 0 1
for n in 1 2; do
  commit
  run 'git push' >/dev/null
done
commit
verdict 0 0 1
r=$(run 'git push')
check "last-round pass with suggestions" "deny: Review round 3 of 3 approved HEAD" "$r"
check "last round says not to apply" yes "$([[ "$r" == *"do not apply them: file one cleanup issue"* ]] && echo yes)"
check "last-round approved HEAD pushes" allow "$(run 'git push')"

# --- base branch from git config ---
new_repo e 4-stacked
commit
git -C "$repo" push -q origin 4-stacked:3-parent
git -C "$repo" config branch.4-stacked.reviewBase 3-parent
check "stacked branch has nothing new vs its base" allow "$(run 'git push')"
check "no review against stacked base" 0 "$(calls)"
git -C "$repo" config branch.4-stacked.reviewBase nope
check "unknown base denied" "deny: review-gate: base branch 'nope'" "$(run 'git push')"

# --- fails closed ---
new_repo f 5-broken
commit
echo 'not json' >"$work/verdict"
check "unreadable verdict" "deny: review-gate: could not read the review verdict" "$(run 'git push')"
jq -n '{structured_output: {critical: 0, important: 0, report: "r"}}' >"$work/verdict"
check "verdict without suggestions" "deny: review-gate: could not read the review verdict" "$(run 'git push')"
echo 1 >"$work/stub-exit"
check "review crash" "deny: review-gate: the headless review failed (exit 1" "$(run 'git push')"
check "scratch dir removed after failed review" gone "$(t=$(cat "$work/last-target"); [ -e "${t%/target}" ] || echo gone)"
command rm -f -- "$work/stub-exit"
verdict 0 0
check "recovers after failure" allow "$(run 'git push')"

# --- a reviewer stopped before reporting fails closed ---
# A task's start and its stopped notification, as `claude -p` logs them.
task() { # id task_type summary
  jq -c -n --arg id "$1" --arg t "$2" --arg s "$3" \
    '{type: "system", subtype: "task_started", task_id: $id, task_type: $t, description: $s},
     {type: "system", subtype: "task_notification", task_id: $id, status: "stopped", summary: $s}'
}
new_repo k 50-partial
commit
verdict 0 0
{ task a1 local_agent "General code review"; task a2 local_agent "Test coverage review"; } >"$work/events"
r=$(run 'git push')
check "stopped agents deny" "deny: review-gate: review agent(s) stopped before reporting (General code review, Test coverage review)" "$r"
check "stopped review is no round" no "$([ -e "$(git -C "$repo" rev-parse --absolute-git-dir)/review-gate/50-partial/rounds" ] && echo yes || echo no)"
task b1 local_bash "Rerun the test suite" >"$work/events"
check "a stopped shell task alone passes" allow "$(run 'git push')"
check "same HEAD reviewed again after the stop" 2 "$(calls)"
command rm -f -- "$work/events"

# --- dependencies are installed in the worktree from its lockfile ---
new_repo m 60-deps
commit
verdict 0 0
check "no lockfile, no install" allow "$(run 'git push')"
check "npm not run without a lockfile" 0 "$(npm_calls)"
echo '{}' >"$repo/package-lock.json"
git -C "$repo" add package-lock.json
git -C "$repo" -c user.name=t -c user.email=t@example.invalid commit -q -m lock
check "lockfile, installed" allow "$(run 'git push')"
check "npm ci run once" 1 "$(npm_calls)"
check "npm ci in the worktree" "$work/review-gate." "$(tail -1 "$work/npm-calls")"
check "npm ci from the lockfile" yes "$(command grep -q ' ci --prefer-offline' "$work/npm-calls" && echo yes)"
check "reviewer told dependencies are installed" yes "$(command grep -q 'Dependencies are installed' "$work/last-args" && echo yes)"
commit
echo 1 >"$work/npm-exit"
check "failed install still reviews" allow "$(run 'git push')"
check "reviewer told the install failed" yes "$(command grep -q 'Installing dependencies (npm ci) failed' "$work/last-args" && echo yes)"
check "install log kept" yes "$([ -e "$(git -C "$repo" rev-parse --absolute-git-dir)/review-gate/60-deps/npm-ci.log" ] && echo yes)"
command rm -f -- "$work/npm-exit"

# --- a later round checks the last one's findings and reviews only what's new ---
new_repo n 70-rounds
commit
jq -n '{type: "result", structured_output: {critical: 0, important: 1, suggestions: 1,
  report: "# Summary\n## Important Issues (1 found)\n1. important-marker\n## Suggestions (1 found)\n1. suggestion-marker"}}' >"$work/verdict"
check "round 1 fails" "deny: Review round 1 of 3 found" "$(run 'git push')"
check "round 1 is a full review" no "$(command grep -q 'This is round' "$work/last-args" && echo yes || echo no)"
check "missing tests graded by reach" yes "$(command grep -q 'A missing test is Important only when' "$work/last-args" && echo yes)"
round1=$(git -C "$repo" rev-parse HEAD)
commit
check "round 2 fails" "deny: Review round 2 of 3 found" "$(run 'git push')"
check "round 2 told it is a follow-up" yes "$(command grep -q 'This is round 2 of 3' "$work/last-args" && echo yes)"
check "round 2 scoped to new commits" yes "$(command grep -qF "git diff $round1 $(git -C "$repo" rev-parse HEAD)" "$work/last-args" && echo yes)"
check "round 2 gets round 1's findings" yes "$(command grep -q 'important-marker' "$work/last-args" && echo yes)"
check "round 2 doesn't get round 1's suggestions" no "$(command grep -q 'suggestion-marker' "$work/last-args" && echo yes || echo no)"
# A rebase drops the reviewed commit from HEAD's history: review it all
git -C "$repo" reset -q --hard main
commit
check "round 3 after a rebase" "deny: Review round 3 of 3 (the cap)" "$(run 'git push')"
check "rebased branch gets a full review" no "$(command grep -q 'This is round' "$work/last-args" && echo yes || echo no)"

check "no scratch dirs left behind" 0 "$(find "$work" -maxdepth 1 -name 'review-gate-scratch.*' | wc -l)"
check "no worktrees left behind" 0 "$(find "$work" -maxdepth 1 -name 'review-gate.*' | wc -l)"

echo "passed: $pass  failed: $fail"
[ "$fail" -eq 0 ]
