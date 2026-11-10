#!/usr/bin/env bash
# PreToolUse hook (matcher: Bash): gate `git push` on ticket branches behind a
# headless /pr-review-toolkit:review-pr run.
#
# A ticket branch is one named `<issue-number>-<slug>`, the convention used by
# /implement-ticket. A push is gated when a ticket branch is involved: it is
# checked out, or the push's refspecs name one as source or destination.
# A gated push may only send the checked-out ticket branch to its own name
# (no other refspecs, --all/--mirror/--tags, or force), so the commit
# reviewed is exactly the commit pushed. Other pushes pass untouched.
#
# For a gated push the hook reviews HEAD against the branch's base:
#   * HEAD already approved              -> allow
#   * HEAD already reviewed with findings -> deny again with the same report
#   * otherwise                           -> run the review (one "round")
# From the second round on, the review checks the previous round's Critical
# and Important findings and reviews only the commits since that round.
# A round with no Critical or Important findings approves HEAD; if it left
# Suggestions, that push is denied once so a cleanup issue gets filed for the
# ones not applied, and the next push of the approved HEAD goes through.
# After MAX_ROUNDS failing rounds the branch is "capped": the push is denied
# once more with the open findings, then allowed. Either way, what is left
# goes in a cleanup issue, not only in a PR comment.
#
# The review runs in a detached worktree of HEAD with write tools disabled
# and all hooks off, so it cannot touch the branch or re-enter this gate.
#
# State lives in <git-common-dir>/review-gate/<branch>/:
#   rounds         number of review rounds run
#   approved       sha that passed review
#   last-sha       sha of the latest review
#   last-verdict   pass | fail
#   last-critical  number of Critical issues the latest review found
#   capped        present once MAX_ROUNDS failing rounds have run
#   round-N.md     full report of round N
#   review-<time>.log  stream-json events of one review attempt, kept even
#                      when it times out or fails
#   reviews.log    one line per attempt: start, end, seconds, exit, sha
#
# The base branch comes from `git config branch.<branch>.reviewBase`
# (written by /implement-ticket), falling back to `main`.
#
# The check fails CLOSED: if the review cannot run or its verdict cannot be
# read, the push is denied.

set -u

MAX_ROUNDS=${REVIEW_GATE_MAX_ROUNDS:-3}
REVIEW_TIMEOUT=${REVIEW_GATE_TIMEOUT:-1500}
# How long the event log must sit unchanged, once a verdict is in and no
# background agent is left, before the review is taken as finished.
REVIEW_IDLE=${REVIEW_GATE_IDLE:-60}
CLAUDE_BIN=${REVIEW_GATE_CLAUDE:-claude}

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

if ! command -v jq >/dev/null 2>&1; then
  printf 'review-gate hook: jq not found, so the push could not be checked\n' >&2
  exit 2
fi

input=$(cat)
[ "$(jq -r '.tool_name // ""' <<<"$input")" = "Bash" ] || exit 0
cmd=$(jq -r '.tool_input.command // ""' <<<"$input")
cwd=$(jq -r '.cwd // ""' <<<"$input")

# Only commands whose git subcommand is `push` are of interest. Global
# options (`-c k=v`, `-C dir`) may sit between `git` and `push`; any other
# word there means a different subcommand, so `git commit -m 'push'` passes.
grep -qE '(^|[^[:alnum:]_.-])git([[:space:]]+-[^[:space:]]+([[:space:]]+[^-[:space:]][^[:space:]]*)?)*[[:space:]]+push([[:space:]]|$)' <<<"$cmd" || exit 0

is_ticket() { [[ "$1" =~ ^[0-9]+- ]]; }

[ -n "$cwd" ] && cd -- "$cwd" 2>/dev/null || exit 0
in_repo=0
git rev-parse --git-dir >/dev/null 2>&1 && in_repo=1
branch=""
[ "$in_repo" = 1 ] && branch=$(git symbolic-ref --short -q HEAD)

# The hook reviews HEAD as it is now, before the command runs, and works out
# what is pushed by reading the command's words. Anything that could move
# HEAD first (`git commit && git push`), point git at another repo, or that
# plain word splitting cannot read reliably (quotes, escapes, expansions) is
# refused whenever a ticket branch could be involved: the current branch is
# one, or the command names one.
if grep -qE '&&|\|\||;|\||`|\$|[\\"'\''<>]|[[:space:]]-C[[:space:]]|--git-dir|--work-tree' <<<"$cmd" ||
  [ "$(printf '%s\n' "$cmd" | wc -l)" -gt 1 ]; then
  if is_ticket "$branch" || grep -qE '(^|[^[:alnum:]_.-])[0-9]+-' <<<"$cmd"; then
    deny "A push involving a ticket branch must be a plain \`git push\` run as a command of its own, from the repo root: no -C/--git-dir, no quotes or \$-expansions, and nothing chained before or after it. The review gate reviews HEAD as it is when the push starts."
  fi
  exit 0
fi
[ "$in_repo" = 1 ] || exit 0

# Split the push into options, remote, and refspecs.
read -ra words <<<"$cmd"
i=0
while [ "$i" -lt "${#words[@]}" ] && [ "${words[$i]}" != "push" ]; do i=$((i + 1)); done
remote=""
refspecs=()
bulk=""    # set when the push sends refs the hook cannot pin down
force=0
delete=0
for ((i = i + 1; i < ${#words[@]}; i++)); do
  w=${words[$i]}
  case "$w" in
    --all | --branches | --mirror | --tags) bulk=$w ;;
    --force | --force-with-lease* | --force-if-includes) force=1 ;;
    --delete) delete=1 ;;
    --dry-run) exit 0 ;;
    --repo) i=$((i + 1)); remote=${words[$i]:-} ;;
    --repo=*) remote=${w#--repo=} ;;
    -o | --push-option | --receive-pack | --exec) i=$((i + 1)) ;;
    --*) ;;
    -*)
      # Clustered short flags, e.g. -uf.
      [[ "$w" == *f* ]] && force=1
      [[ "$w" == *d* ]] && delete=1
      [[ "$w" == *n* ]] && exit 0
      ;;
    *)
      if [ -z "$remote" ]; then remote=$w; else refspecs+=("$w"); fi
      ;;
  esac
done

# Deleting remote branches (Step 2 of /implement-ticket cleans up empty ones)
# pushes no code, so it needs no review.
[ "$delete" = 1 ] && exit 0

# Resolve each refspec to a source commit and destination branch name.
# Without refspecs, git follows remote.<remote>.push, then push.default.
srcs=()
dsts=()
short() { local r=${1#refs/heads/}; printf '%s' "${r#heads/}"; }
if [ "${#refspecs[@]}" -eq 0 ] && [ -z "$bulk" ]; then
  if [ -n "$(git config --get-regexp '^remote\..*\.push$' 2>/dev/null)" ]; then
    bulk="remote.<name>.push"
  else
    mode=$(git config --get push.default || echo simple)
    case "$mode" in
      matching) bulk="push.default=matching" ;;
      nothing) exit 0 ;;
      upstream | tracking)
        [ -n "$branch" ] || exit 0
        srcs+=("$branch")
        dsts+=("$(short "$(git config --get "branch.$branch.merge" || echo "$branch")")")
        ;;
      *)
        [ -n "$branch" ] || exit 0
        srcs+=("$branch")
        dsts+=("$branch")
        ;;
    esac
  fi
fi
for spec in "${refspecs[@]}"; do
  if [[ "$spec" == +* ]]; then force=1; spec=${spec#+}; fi
  if [[ "$spec" == ^* || "$spec" == *\** ]]; then bulk=$spec; continue; fi
  src=${spec%%:*}
  dst=${spec#*:}
  [ -z "$src" ] && continue # `:dst` deletes dst
  if [ "$src" = "HEAD" ] && [ "$dst" = "$spec" ]; then dst=$branch; fi
  srcs+=("$src")
  dsts+=("$(short "$dst")")
done

# Only `:dst` deletes: nothing is pushed.
[ "${#srcs[@]}" -eq 0 ] && [ -z "$bulk" ] && exit 0

# Is a ticket branch involved anywhere?
gated=0
is_ticket "$branch" && gated=1
for n in "${srcs[@]}" "${dsts[@]}"; do is_ticket "$(short "$n")" && gated=1; done
if [ -n "$bulk" ] && git for-each-ref --format='%(refname:short)' refs/heads/ | grep -qE '^[0-9]+-'; then
  gated=1
fi
[ "$gated" = 1 ] || exit 0

# A gated push may only send the checked-out ticket branch to its own name,
# so the commit reviewed (HEAD) is exactly the commit pushed.
only_current="Check out the ticket branch you want to push and run a plain \`git push\` (or \`git push -u origin <branch>\`) from it."
[ -n "$bulk" ] && deny "review-gate: '$bulk' would push branches the gate cannot pin down, and a ticket branch is involved. $only_current"
[ "$force" = 1 ] && deny "review-gate: force pushes are not allowed when a ticket branch is involved."
is_ticket "$branch" || deny "review-gate: this push sends to ticket branch(es) ${dsts[*]} from '${branch:-detached HEAD}', which skips the review. $only_current"
head=$(git rev-parse HEAD) || deny "review-gate: could not resolve HEAD."
for ((k = 0; k < ${#srcs[@]}; k++)); do
  sha=$(git rev-parse -q --verify "${srcs[$k]}^{commit}") ||
    deny "review-gate: cannot resolve '${srcs[$k]}' to a commit."
  if [ "$sha" != "$head" ] || [ "${dsts[$k]}" != "$branch" ]; then
    deny "review-gate: on ticket branch '$branch', this push sends '${srcs[$k]}' to '${dsts[$k]}'. Only '$branch' may be pushed, to its own name, so the reviewed commit is the one pushed. $only_current"
  fi
done

base=$(git config --get "branch.$branch.reviewBase" || true)
base=${base:-main}
base_ref=$base
git rev-parse -q --verify "origin/$base^{commit}" >/dev/null && base_ref="origin/$base"
git rev-parse -q --verify "$base_ref^{commit}" >/dev/null ||
  deny "review-gate: base branch '$base' not found locally or on origin. Set it with \`git config branch.$branch.reviewBase <base>\`."
merge_base=$(git merge-base "$base_ref" HEAD) ||
  deny "review-gate: no merge base between $base_ref and HEAD."

# Nothing to review.
[ "$merge_base" = "$head" ] && exit 0

state=$(git rev-parse --path-format=absolute --git-common-dir)/review-gate/$branch
mkdir -p -- "$state" || deny "review-gate: could not create $state."

# One review at a time per branch; a second push waits for the first.
exec 9>"$state/lock"
flock 9

read_state() { cat -- "$state/$1" 2>/dev/null; }

[ "$(read_state approved)" = "$head" ] && exit 0
[ -e "$state/capped" ] && exit 0

rounds=$(read_state rounds)
rounds=${rounds:-0}

if [ "$(read_state last-sha)" = "$head" ] && [ "$(read_state last-verdict)" = "fail" ]; then
  deny "HEAD $head was already reviewed (round $rounds of $MAX_ROUNDS) and has open Critical/Important findings. Fix them, commit, and push again. Full report: $state/round-$rounds.md"
fi

# Run the review in a throwaway worktree of HEAD. Builds and temp files the
# review makes go in a scratch dir of their own, removed with the worktree,
# so per-review cargo targets (~2 GB each) do not pile up. Both live on disk
# (REVIEW_GATE_DIR), not in /tmp, which is often tmpfs: a worktree with its
# node_modules installed is ~700 MB, which would otherwise sit in RAM.
base=${REVIEW_GATE_DIR:-${XDG_CACHE_HOME:-$HOME/.cache}/review-gate}
mkdir -p -- "$base" || deny "review-gate: could not create $base."
wt=$(mktemp -d "$base/review-gate.XXXXXX") || deny "review-gate: mktemp failed."
scratch=$(mktemp -d "$base/review-gate-scratch.XXXXXX") || {
  rmdir "$wt"
  deny "review-gate: mktemp failed."
}
cleanup() {
  git worktree remove --force "$wt" >/dev/null 2>&1
  rmdir "$wt" 2>/dev/null
  rm -rf -- "$scratch"
}
trap cleanup EXIT
git worktree add --detach --quiet "$wt" "$head" >/dev/null 2>&1 ||
  deny "review-gate: could not create a review worktree."

# node_modules is git-ignored, so a fresh worktree has none, and every test,
# lint and typecheck the review runs would fail on a missing tool. npm ci
# installs from the reviewed commit's own lockfile; linking the checkout's
# node_modules instead would point the workspace packages at the checkout's
# files, not the commit under review. A failed install doesn't block the
# review: the reviewer is told, so it reports that instead of tool failures.
deps_note=
if [ -f "$wt/package-lock.json" ]; then
  if (cd -- "$wt" && TMPDIR=$scratch timeout 600 npm ci --prefer-offline --no-audit --no-fund) \
    >"$state/npm-ci.log" 2>&1; then
    deps_note="Dependencies are installed (npm ci from the commit's package-lock.json), so the test suite, lint and typecheck can run."
  else
    deps_note="Installing dependencies (npm ci) failed, see $state/npm-ci.log, so tools from node_modules are missing. Report that once as an environment problem; do not report each failed tool run as a finding."
  fi
fi

# A later round checks the previous round's Critical and Important findings
# and reviews only the commits since, so fixing a round doesn't start a fresh
# review of the whole branch, which finds new gaps every time. Without the
# previous round's commit in HEAD's history (a rebase), or its report, the
# round reviews the whole branch.
followup_note=
prev_sha=$(read_state last-sha)
prev_report=$state/round-$rounds.md
if [ "$rounds" -gt 0 ] && [ -n "$prev_sha" ] && [ -f "$prev_report" ] &&
  git merge-base --is-ancestor "$prev_sha" "$head" 2>/dev/null; then
  prev_findings=$(awk '/^## (Suggestions|Strengths)/ { exit } { print }' "$prev_report")
  followup_note="This is round $((rounds + 1)) of $MAX_ROUNDS. Round $rounds reviewed $prev_sha, and this round has two jobs only. (1) Check that each Critical and Important finding of round $rounds, quoted below, is fixed; one that isn't keeps its rating. (2) Review the branch's commits since then for defects they introduce: \`git log --no-merges $prev_sha..$head\` and \`git diff $prev_sha $head\` (changes merged in from the base branch are not under review). Use the whole branch's diff only for context. A new finding in code those commits didn't change is a Suggestion, unless it is Critical.

Round $rounds's findings:
$prev_findings"
fi

schema='{
  "type": "object",
  "properties": {
    "critical": {"type": "integer", "minimum": 0},
    "important": {"type": "integer", "minimum": 0},
    "suggestions": {"type": "integer", "minimum": 0},
    "report": {"type": "string"}
  },
  "required": ["critical", "important", "suggestions", "report"]
}'

prompt="/pr-review-toolkit:review-pr all parallel

Review scope: the committed changes on branch '$branch' against its base, i.e. \`git diff $merge_base...$head\` (list files with \`git diff --name-only $merge_base...$head\`). Ignore the working tree and do not look for a GitHub PR; this repo is on Forgejo. You are read-only: do not modify any file. If you build or run tests, keep the CARGO_TARGET_DIR and TMPDIR already set in the environment; do not create target or temp directories anywhere else, such as under /tmp. Run the test suite at most once in the whole review, and only in the pr-test-analyzer agent: tell every other agent not to run tests. Concurrent runs share one test database, collide, and fail for reasons unrelated to the change.${deps_note:+ $deps_note}

Severity decides whether this push goes through, so grade these two kinds of finding as Suggestions, however an agent rated them: (1) design questions that need the maintainer to decide, which another fix round can't settle (start each with \"Decision needed:\"); (2) the accuracy or wording of comments and documentation. Defects in what the code does keep their Critical or Important rating. A missing test is Important only when a behaviour the change adds that a user can reach, including how it is refused or fails, has no test at all; another case for a behaviour that is already tested (one more edge value, a stronger fixture, a branch no user can reach) is a Suggestion.${followup_note:+

$followup_note}

Return the full PR Review Summary markdown in \`report\`, and the number of Critical issues, Important issues and Suggestions in \`critical\`, \`important\` and \`suggestions\`."

# Every attempt streams its events (each tool call and result, from every
# agent) to a log of its own, so a review that times out still shows what it
# was doing. reviews.log keeps one line per attempt: start, end, exit, sha.
attempt=$(date +%Y%m%dT%H%M%S)
log=$state/review-$attempt.log
started=$(date +%s)

# "yes" once the main session has submitted a verdict (StructuredOutput), no
# background agent is outstanding, and a turn has ended since the verdict.
done_jq='[split("\n")[] | fromjson? | select(type == "object")] as $ev
| ([$ev | to_entries[]
    | select(.value.type == "assistant" and .value.parent_tool_use_id == null
        and any(.value.message.content[]?; .type == "tool_use" and .name == "StructuredOutput"))
    | .key] | last) as $v
| ([$ev[] | select(.type == "system" and .subtype == "background_tasks_changed")] | last) as $bg
| if $v != null and ($bg == null or ($bg.tasks | length) == 0)
     and any($ev[$v:][]; .type == "result")
  then "yes" else "no" end'
# The main session's last StructuredOutput: a background agent finishing
# after the verdict wakes the session, which may submit a revised one.
verdict_jq='[split("\n")[] | fromjson? | select(type == "object")
  | select(.type == "assistant" and .parent_tool_use_id == null)
  | .message.content[]? | select(.type == "tool_use" and .name == "StructuredOutput")
  | .input] | last // empty'

# The review runs in the background so the gate can stop it once it is done:
# a finished session can stay alive (a scheduled wakeup, a late notification)
# until the timeout, which would fail a review that had in fact finished.
# Without the ceiling of 0, `claude -p` stops reviewers still running 10
# minutes after its last turn, and the verdict is written without them;
# REVIEW_TIMEOUT still bounds the whole review.
(cd -- "$wt" && CARGO_TARGET_DIR=$scratch/target TMPDIR=$scratch \
  CLAUDE_CODE_PRINT_BG_WAIT_CEILING_MS=0 \
  exec timeout --kill-after=30 "$REVIEW_TIMEOUT" "$CLAUDE_BIN" -p "$prompt" \
  --output-format stream-json \
  --verbose \
  --json-schema "$schema" \
  --settings '{"disableAllHooks": true}' \
  --permission-mode dontAsk \
  --allowedTools 'Read,Grep,Glob,Task,Bash(git diff *),Bash(git log *),Bash(git show *),Bash(git status *),Bash(git rev-parse *),Bash(git ls-files *)' \
  --disallowedTools 'Edit,Write,NotebookEdit,ScheduleWakeup,CronCreate,CronDelete,Monitor,RemoteTrigger' \
  --no-session-persistence >"$log" 2>"$state/last-stderr") &
review_pid=$!
stopped=
while kill -0 "$review_pid" 2>/dev/null; do
  sleep 5
  idle=$(($(date +%s) - $(stat -c %Y "$log" 2>/dev/null || date +%s)))
  if [ "$idle" -ge "$REVIEW_IDLE" ] && [ "$(jq -R -s -r "$done_jq" "$log" 2>/dev/null)" = yes ]; then
    stopped=1
    kill "$review_pid" 2>/dev/null
    break
  fi
done
wait "$review_pid"
status=$?
# Stopped by the gate after a finished review: not a failure.
[ -n "$stopped" ] && status=0
printf '%s start=%s end=%s seconds=%s exit=%s sha=%s log=%s\n' \
  "$attempt" "$(date -d "@$started" +%T)" "$(date +%T)" "$(($(date +%s) - started))" \
  "$status" "$head" "$log" >>"$state/reviews.log"

if [ "$status" -ne 0 ]; then
  # The last few tool calls show where a hung or failed review was stuck.
  last_calls=$(jq -r 'select(.type == "assistant") | .message.content[]?
      | select(.type == "tool_use")
      | "  \(.name): \(.input.command // .input.description // .input.file_path // .input.pattern // "" | tostring | .[0:160])"' \
    "$log" 2>/dev/null | tail -n 5)
  deny "review-gate: the headless review failed (exit $status$([ "$status" -eq 124 ] && echo ", timed out after ${REVIEW_TIMEOUT}s")). The push was blocked rather than allowed unreviewed. Event log: $log, stderr: $state/last-stderr
Last tool calls:
${last_calls:-  (none logged)}"
fi

# A reviewer agent stopped before it reported leaves a verdict written
# without it, so deny, as for a timeout. Stopped shell tasks (a test run a
# reviewer gave up on) don't count.
stopped_jq='[split("\n")[] | fromjson? | select(type == "object" and .type == "system")] as $ev
| [$ev[] | select(.subtype == "task_started" and .task_type == "local_agent") | .task_id] as $agents
| [$ev[] | select(.subtype == "task_notification" and .status == "stopped")
    | select(.task_id as $t | any($agents[]; . == $t)) | .summary // .task_id]
| join(", ")'
stopped_agents=$(jq -R -s -r "$stopped_jq" "$log" 2>/dev/null)
[ -z "$stopped_agents" ] ||
  deny "review-gate: review agent(s) stopped before reporting ($stopped_agents), so the verdict is incomplete and the push was blocked. Event log: $log"

out=$(jq -c 'select(.type == "result")' "$log" 2>/dev/null | tail -n 1)
verdict=$(jq -R -s -c "$verdict_jq" "$log" 2>/dev/null)
[ -n "$verdict" ] || verdict=$(jq -c '.structured_output // empty' <<<"$out" 2>/dev/null)
critical=$(jq -r '.critical // empty' <<<"$verdict" 2>/dev/null)
important=$(jq -r '.important // empty' <<<"$verdict" 2>/dev/null)
suggestions=$(jq -r '.suggestions // empty' <<<"$verdict" 2>/dev/null)
if ! [[ "$critical" =~ ^[0-9]+$ && "$important" =~ ^[0-9]+$ && "$suggestions" =~ ^[0-9]+$ ]]; then
  printf '%s\n' "$out" >"$state/last-output.json"
  deny "review-gate: could not read the review verdict, so the push was blocked. Result event: $state/last-output.json, full event log: $log"
fi

rounds=$((rounds + 1))
report_file=$state/round-$rounds.md
jq -r '.report' <<<"$verdict" >"$report_file"
printf '%s\n' "$rounds" >"$state/rounds"
printf '%s\n' "$head" >"$state/last-sha"
printf '%s\n' "$critical" >"$state/last-critical"

if [ "$critical" -eq 0 ] && [ "$important" -eq 0 ]; then
  printf 'pass\n' >"$state/last-verdict"
  printf '%s\n' "$head" >"$state/approved"
  [ "$suggestions" -eq 0 ] && exit 0
  if [ "$rounds" -ge "$MAX_ROUNDS" ]; then
    choice="This was the last review round, so do not apply them: file one cleanup issue for all of them"
  else
    choice="Apply the cheap, clearly correct ones (a new commit starts round $((rounds + 1)) of $MAX_ROUNDS), and file one cleanup issue for every one you leave"
  fi
  deny "Review round $rounds of $MAX_ROUNDS approved HEAD $head with $suggestions Suggestion(s). $choice, with a dependency on this branch's ticket, and link it in the PR Review Summary. A PR comment alone does not track them. Then push again: this HEAD is approved and goes through. Full report: $report_file

$(cat -- "$report_file")"
fi

printf 'fail\n' >"$state/last-verdict"
report=$(cat -- "$report_file")

if [ "$rounds" -ge "$MAX_ROUNDS" ]; then
  : >"$state/capped"
  deny "Review round $rounds of $MAX_ROUNDS (the cap) still found $critical Critical and $important Important issue(s). Do not start another fix round. Record these as \"open after the review cap\" in the PR Review Summary and file one cleanup issue for them and the round's Suggestions, with a dependency on this branch's ticket; a PR comment alone does not track them. Then push again: the gate will let it through. Full report: $report_file

$report"
fi

deny "Review round $rounds of $MAX_ROUNDS found $critical Critical and $important Important issue(s). Fix each one, rerun the affected tests, commit each fix on '$branch', then push again to trigger the next round. Full report: $report_file

$report"
