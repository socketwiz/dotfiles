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
# A round with no Critical or Important findings approves HEAD. After
# MAX_ROUNDS failing rounds the branch is "capped": the push is denied once
# more with the open findings, then allowed, so the PR can carry them.
#
# The review runs in a detached worktree of HEAD with write tools disabled
# and all hooks off, so it cannot touch the branch or re-enter this gate.
#
# State lives in <git-common-dir>/review-gate/<branch>/:
#   rounds         number of review rounds run
#   approved       sha that passed review
#   last-sha       sha of the latest review
#   last-verdict   pass | fail
#   capped         present once MAX_ROUNDS failing rounds have run
#   round-N.md     full report of round N
#
# The base branch comes from `git config branch.<branch>.reviewBase`
# (written by /implement-ticket), falling back to `main`.
#
# The check fails CLOSED: if the review cannot run or its verdict cannot be
# read, the push is denied.

set -u

MAX_ROUNDS=${REVIEW_GATE_MAX_ROUNDS:-3}
REVIEW_TIMEOUT=${REVIEW_GATE_TIMEOUT:-1500}
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

# Run the review in a throwaway worktree of HEAD.
wt=$(mktemp -d "${TMPDIR:-/tmp}/review-gate.XXXXXX") || deny "review-gate: mktemp failed."
cleanup() { git worktree remove --force "$wt" >/dev/null 2>&1; rmdir "$wt" 2>/dev/null; }
trap cleanup EXIT
git worktree add --detach --quiet "$wt" "$head" >/dev/null 2>&1 ||
  deny "review-gate: could not create a review worktree."

schema='{
  "type": "object",
  "properties": {
    "critical": {"type": "integer", "minimum": 0},
    "important": {"type": "integer", "minimum": 0},
    "report": {"type": "string"}
  },
  "required": ["critical", "important", "report"]
}'

prompt="/pr-review-toolkit:review-pr all parallel

Review scope: the committed changes on branch '$branch' against its base, i.e. \`git diff $merge_base...$head\` (list files with \`git diff --name-only $merge_base...$head\`). Ignore the working tree and do not look for a GitHub PR; this repo is on Forgejo. You are read-only: do not modify any file.

Return the full PR Review Summary markdown in \`report\`, and the number of Critical and Important issues in \`critical\` and \`important\`."

out=$(cd -- "$wt" && timeout "$REVIEW_TIMEOUT" "$CLAUDE_BIN" -p "$prompt" \
  --output-format json \
  --json-schema "$schema" \
  --settings '{"disableAllHooks": true}' \
  --permission-mode dontAsk \
  --allowedTools 'Read,Grep,Glob,Task,Bash(git diff *),Bash(git log *),Bash(git show *),Bash(git status *),Bash(git rev-parse *),Bash(git ls-files *)' \
  --disallowedTools 'Edit,Write,NotebookEdit' \
  --no-session-persistence 2>"$state/last-stderr")
status=$?

if [ "$status" -ne 0 ]; then
  deny "review-gate: the headless review failed (exit $status$([ "$status" -eq 124 ] && echo ", timed out after ${REVIEW_TIMEOUT}s")). The push was blocked rather than allowed unreviewed. stderr: $state/last-stderr"
fi

verdict=$(jq -c '.structured_output // empty' <<<"$out" 2>/dev/null)
critical=$(jq -r '.critical // empty' <<<"$verdict" 2>/dev/null)
important=$(jq -r '.important // empty' <<<"$verdict" 2>/dev/null)
if ! [[ "$critical" =~ ^[0-9]+$ && "$important" =~ ^[0-9]+$ ]]; then
  printf '%s\n' "$out" >"$state/last-output.json"
  deny "review-gate: could not read the review verdict, so the push was blocked. Raw output: $state/last-output.json"
fi

rounds=$((rounds + 1))
report_file=$state/round-$rounds.md
jq -r '.report' <<<"$verdict" >"$report_file"
printf '%s\n' "$rounds" >"$state/rounds"
printf '%s\n' "$head" >"$state/last-sha"

if [ "$critical" -eq 0 ] && [ "$important" -eq 0 ]; then
  printf 'pass\n' >"$state/last-verdict"
  printf '%s\n' "$head" >"$state/approved"
  exit 0
fi

printf 'fail\n' >"$state/last-verdict"
report=$(cat -- "$report_file")

if [ "$rounds" -ge "$MAX_ROUNDS" ]; then
  : >"$state/capped"
  deny "Review round $rounds of $MAX_ROUNDS (the cap) still found $critical Critical and $important Important issue(s). Do not start another fix round. Record these as \"open after the review cap\" in the PR Review Summary and file the follow-up issue, then push again: the gate will let it through. Full report: $report_file

$report"
fi

deny "Review round $rounds of $MAX_ROUNDS found $critical Critical and $important Important issue(s). Fix each one, rerun the affected tests, commit each fix on '$branch', then push again to trigger the next round. Full report: $report_file

$report"
