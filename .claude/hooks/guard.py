#!/usr/bin/env python3
"""PreToolUse guard for Bash and outbound tools.

Checks, one function each:
  * check_kill      kill/pkill/killall must not hit PID 1, an ancestor of
                    this Claude session, a session-critical process, another
                    user's process, a process group, or a computed PID; and
                    commands that end the login session are refused.
  * check_rm        rm/rmdir/unlink/shred only take fully qualified literal
                    paths (no variables, globs, `..`, relative paths, or
                    top-level directories); `find -delete`, `find -exec rm`
                    and `xargs rm` are refused.
  * check_commit    `git commit` messages follow ~/.claude/CLAUDE.md:
                    title <= 72 chars, imperative, no trailing period, blank
                    line, then a `*` bulleted body wrapped at 72.
  * check_outbound  requests to external services (curl/wget/..., WebFetch,
                    WebSearch, browser MCP tools) must not carry the user's
                    identifiers, listed one per line in
                    ~/.claude/hooks/private-identifiers (kept out of git).
                    This one asks the user instead of denying.

Shell commands are parsed with shlex after removing heredoc bodies, then
split into simple commands at ; && || | & ( ). Wrappers such as sudo, env,
timeout and xargs are peeled off, and `bash -c`/`eval` strings are checked
recursively.

An internal error never blocks the call: it is logged to stderr (shown as a
non-blocking hook error) and the call proceeds.
"""

import json
import os
import re
import shlex
import subprocess
import sys
from pathlib import Path
from urllib.parse import unquote_plus

HOME = os.path.expanduser("~")
IDENTIFIERS_FILE = os.environ.get(
    "GUARD_IDENTIFIERS_FILE", os.path.join(HOME, ".claude/hooks/private-identifiers")
)


class Decision(Exception):
    def __init__(self, kind, reason):
        super().__init__(reason)
        self.kind = kind  # "deny" or "ask"
        self.reason = reason


def deny(reason):
    raise Decision("deny", reason)


# ---------------------------------------------------------------- parsing

HEREDOC_RE = re.compile(r"<<(-?)[ \t]*(['\"]?)([A-Za-z_][A-Za-z0-9_]*)\2")
PLACEHOLDER_RE = re.compile(r"__HEREDOC_(\d+)__")


def preprocess(cmd):
    """Make a command shlex-friendly, tracking quotes the way the shell does.

    * unquoted newlines become `;` (command boundaries)
    * unquoted `# comments` are dropped
    * backslash-newline continuations are joined
    * each heredoc marker outside single quotes (also inside "$(cat <<EOF"),
      becomes a `__HEREDOC_<n>__` placeholder, and its body is cut out

    Returns (text, bodies) where bodies[n] is heredoc n's body.
    """
    out, bodies, pending = [], {}, []
    i, n, quote = 0, len(cmd), None
    while i < n:
        ch = cmd[i]
        if quote == "'":
            if ch == "'":
                quote = None
            out.append(ch)
            i += 1
            continue
        if ch == "\\" and i + 1 < n:
            if cmd[i + 1] == "\n":
                out.append(" ")
            else:
                out.append(cmd[i:i + 2])
            i += 2
            continue
        if ch == "<" and cmd.startswith("<<", i) and not cmd.startswith("<<<", i) and (i == 0 or cmd[i - 1] != "<"):
            m = HEREDOC_RE.match(cmd, i)
            if m:
                ident = len(bodies) + len(pending)
                pending.append((m.group(1) == "-", m.group(3), ident))
                out.append(f"<<__HEREDOC_{ident}__")
                i = m.end()
                continue
        if ch == "\n":
            out.append(";" if quote is None else "\n")
            i += 1
            for strip_tabs, term, ident in pending:
                body = []
                while i < n:
                    j = cmd.find("\n", i)
                    line = cmd[i:] if j < 0 else cmd[i:j]
                    i = n if j < 0 else j + 1
                    if (line.lstrip("\t") if strip_tabs else line) == term:
                        break
                    body.append(line)
                bodies[ident] = "\n".join(body)
            pending = []
            continue
        if quote is None:
            if ch == "#" and (i == 0 or cmd[i - 1] in " \t;&|("):
                j = cmd.find("\n", i)
                i = n if j < 0 else j
                continue
            if ch in "'\"":
                quote = ch
        elif ch == '"':
            quote = None
        out.append(ch)
        i += 1
    for _, _, ident in pending:  # marker on the last line, no body
        bodies[ident] = ""
    return "".join(out), bodies


def is_separator(tok):
    return bool(tok) and all(c in ";&|()" for c in tok)


def is_redirect(tok):
    return bool(tok) and all(c in "<>&" for c in tok) and ("<" in tok or ">" in tok)


def simple_commands(text):
    """Split preprocessed text into (argv, heredoc ids) per simple command.
    Raises ValueError if unparsable."""
    lex = shlex.shlex(text, posix=True, punctuation_chars=";&|()<>")
    lex.whitespace_split = True
    lex.commenters = ""
    commands, cur, ids = [], [], []
    toks = list(lex)
    i = 0
    while i < len(toks):
        tok = toks[i]
        if is_redirect(tok):
            if cur and cur[-1].isdigit():
                cur.pop()  # the fd in `2>file`
            if i + 1 < len(toks):
                ids += [int(k) for k in PLACEHOLDER_RE.findall(toks[i + 1])]
            i += 2  # the operator and its target
            continue
        if is_separator(tok):
            if cur:
                commands.append((cur, ids))
            cur, ids = [], []
        else:
            cur.append(tok)
            ids += [int(k) for k in PLACEHOLDER_RE.findall(tok)]
        i += 1
    if cur:
        commands.append((cur, ids))
    return commands


WRAPPERS = {
    "sudo", "doas", "env", "command", "builtin", "exec", "nohup", "nice", "ionice",
    "time", "timeout", "stdbuf", "setsid", "chrt", "taskset", "xargs", "unbuffer",
}
WRAPPER_VALUE_OPTS = {
    "sudo": {"-u", "-g", "-C", "-D", "-h", "-p", "-U", "-r", "-t"},
    "doas": {"-u", "-C"},
    "env": {"-u", "-C", "-S"},
    "nice": {"-n"},
    "ionice": {"-c", "-n", "-p"},
    "timeout": {"-s", "-k", "--signal", "--kill-after"},
    "xargs": {"-I", "-n", "-P", "-L", "-d", "-E", "-s", "-a"},
}
ASSIGNMENT_RE = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*=")
# Reserved words that can precede a command: `do rm ...`, `then kill ...`.
KEYWORDS = {"do", "then", "else", "elif", "if", "while", "until", "!", "{", "}"}


def unwrap(argv):
    """Peel wrappers and assignments. Returns (name, args, from_stdin)."""
    from_stdin = False
    i = 0
    while i < len(argv):
        word = argv[i]
        if ASSIGNMENT_RE.match(word) or word in KEYWORDS:
            i += 1
            continue
        base = os.path.basename(word)
        if base not in WRAPPERS:
            return base, argv[i + 1:], from_stdin
        from_stdin = from_stdin or base == "xargs"
        i += 1
        while i < len(argv):
            opt = argv[i]
            if opt.startswith("-"):
                i += 1
                if opt in WRAPPER_VALUE_OPTS.get(base, ()):
                    i += 1
            elif (base == "env" and ASSIGNMENT_RE.match(opt)) or (
                base == "timeout" and re.match(r"^\d", opt)
            ):
                i += 1
            else:
                break
    return None, [], from_stdin


SHELLS = {"bash", "sh", "zsh", "dash", "ksh"}


def walk(cmd, depth=0):
    """Yield (name, args, from_stdin, heredocs) for every simple command,
    recursing into `sh -c` strings and `eval`. heredocs maps each heredoc
    placeholder id in the command to its body."""
    text, bodies = preprocess(cmd)
    for argv, ids in simple_commands(text):
        name, args, from_stdin = unwrap(argv)
        if name is None:
            continue
        yield name, args, from_stdin, {k: bodies.get(k, "") for k in ids}
        if depth < 3:
            if name in SHELLS and "-c" in args and args.index("-c") + 1 < len(args):
                yield from walk(args[args.index("-c") + 1], depth + 1)
            elif name == "eval":
                yield from walk(" ".join(args), depth + 1)


def dynamic(word):
    return "$" in word or "`" in word


# ------------------------------------------------------------------- kill

PROTECTED_COMM = {
    "systemd", "Hyprland", "hyprland", "uwsm", "sddm", "Xwayland", "Xorg",
    "dbus-daemon", "dbus-broker", "dbus-broker-lau", "pipewire", "pipewire-pulse",
    "wireplumber", "quickshell", "qs", "waybar", "hypridle", "hyprlock", "mako",
    "gnome-keyring-d", "polkit-gnome-au", "sshd", "login", "agetty", "tmux: server",
    "NetworkManager", "tailscaled", "upsmon", "upsd", "claude", "ghostty", "kitty",
    "alacritty", "foot",
}
SESSION_ENDERS = {
    "systemctl": {"exit", "poweroff", "reboot", "halt", "kexec", "soft-reboot", "isolate"},
    "loginctl": {"terminate-session", "terminate-user", "kill-session", "kill-user"},
}
KILL_ADVICE = (
    "Find the exact process with `ps -o pid,ppid,comm --forest` and kill that PID "
    "only. Never kill a process's parent to reap it: an orphan is reparented to "
    "`systemd --user`, so its 'parent' may be your whole login session."
)


def proc_comm(pid):
    try:
        return Path(f"/proc/{pid}/comm").read_text().strip()
    except OSError:
        return None


def ancestors():
    pids, pid = {}, os.getpid()
    while pid > 1:
        try:
            stat = Path(f"/proc/{pid}/stat").read_text()
        except OSError:
            break
        pid = int(stat.rsplit(")", 1)[1].split()[1])
        pids[pid] = proc_comm(pid)
    return pids


def protected_reason(pid, ancestor_pids):
    if pid == 1:
        return "PID 1 (init)"
    comm = proc_comm(pid)
    if comm is None:
        return None  # no such process; kill just fails
    if pid in ancestor_pids:
        return f"PID {pid} ({comm}) is an ancestor of this Claude session"
    if comm in PROTECTED_COMM:
        return f"PID {pid} ({comm}) is session-critical"
    try:
        uid = os.stat(f"/proc/{pid}").st_uid
    except OSError:
        return None
    if uid != os.getuid():
        return f"PID {pid} ({comm}) belongs to another user (uid {uid})"
    return None


def check_pids(pids, what):
    anc = ancestors()
    bad = [r for r in (protected_reason(p, anc) for p in pids) if r]
    if bad:
        deny(f"{what} would signal a protected process: {'; '.join(bad)}. {KILL_ADVICE}")


def kill_targets(args):
    """Targets of `kill`, or None for `kill -l`."""
    targets, i, opts_done = [], 0, False
    while i < len(args):
        a = args[i]
        if not opts_done:
            if a == "--":
                opts_done = True
                i += 1
                continue
            if a in ("-l", "-L", "--list", "--table"):
                return None
            if a in ("-s", "-n", "--signal"):
                i += 2
                continue
            if i == 0 and a.startswith("-") and len(a) > 1:
                i += 1  # the signal: -9, -KILL, -TERM
                continue
        opts_done = True
        targets.append(a)
        i += 1
    return targets


SIGNAL_ARG_RE = re.compile(r"^-(\d+|[A-Z][A-Z0-9+-]*)$")


def pgrep(args):
    try:
        res = subprocess.run(["pgrep", *args], capture_output=True, text=True, timeout=5)
    except (OSError, subprocess.TimeoutExpired) as e:
        deny(f"Could not evaluate which processes this targets ({e}). {KILL_ADVICE}")
    if res.returncode == 1:
        return []
    if res.returncode != 0:
        deny(f"Could not evaluate which processes this targets (pgrep: {res.stderr.strip()}). {KILL_ADVICE}")
    return [int(p) for p in res.stdout.split()]


def pkill_selection(args):
    sel, i = [], 0
    while i < len(args):
        a = args[i]
        if i == 0 and SIGNAL_ARG_RE.match(a):
            pass
        elif a == "--signal":
            i += 1
        elif a.startswith("--signal=") or a in ("-e", "--echo"):
            pass
        else:
            sel.append(a)
        i += 1
    return sel


def killall_selection(args):
    """pgrep invocations equivalent to a killall, or deny."""
    names, flags, i = [], [], 0
    regex = ignore_case = False
    while i < len(args):
        a = args[i]
        if a in ("-s", "--signal", "-o", "--older-than", "-y", "--younger-than", "-n", "--ns", "-Z", "--context"):
            i += 2
            continue
        if a in ("-u", "--user"):
            if i + 1 < len(args):
                flags += ["-u", args[i + 1]]
            i += 2
            continue
        if a in ("-r", "--regexp"):
            regex = True
        elif a in ("-I", "--ignore-case"):
            ignore_case = True
        elif a.startswith("-"):
            pass
        else:
            names.append(a)
        i += 1
    if not names:
        if flags:
            deny(f"`killall -u` with no process name kills every process of that user. {KILL_ADVICE}")
        return []
    calls = []
    for n in names:
        call = list(flags)
        if ignore_case:
            call.append("-i")
        call += [n] if regex else ["-x", n]
        calls.append(call)
    return calls


def check_kill(name, args, from_stdin):
    if name in SESSION_ENDERS and any(a in SESSION_ENDERS[name] for a in args):
        deny(f"`{name} {' '.join(args)}` ends the login session or system. Ask the user to run it themselves.")
    if name in ("shutdown", "reboot", "poweroff", "halt"):
        deny(f"`{name}` ends the login session or system. Ask the user to run it themselves.")
    if name == "hyprctl" and "dispatch" in args and "exit" in args:
        deny("`hyprctl dispatch exit` ends the login session. Ask the user to run it themselves.")
    if name == "uwsm" and args[:1] == ["stop"]:
        deny("`uwsm stop` ends the login session. Ask the user to run it themselves.")

    if name not in ("kill", "pkill", "killall"):
        return
    if from_stdin:
        deny(f"`xargs {name}` computes its targets at run time, so they cannot be checked. {KILL_ADVICE}")
    if any(dynamic(a) for a in args):
        deny(f"`{name}` with a computed target ($VAR, $(...), $PPID) cannot be checked before it runs. Use literal PIDs you have inspected. {KILL_ADVICE}")

    if name == "kill":
        targets = kill_targets(args)
        if targets is None:
            return
        pids = []
        for t in targets:
            if t.startswith("%"):
                continue
            if re.fullmatch(r"-?\d+", t):
                n = int(t)
                if n <= 0:
                    deny(f"`kill {t}` signals a whole process group (or every process). {KILL_ADVICE}")
                pids.append(n)
        check_pids(pids, "This kill")
    elif name == "pkill":
        check_pids(pgrep(pkill_selection(args)), "This pkill")
    else:
        pids = []
        for call in killall_selection(args):
            pids += pgrep(call)
        check_pids(pids, "This killall")


# --------------------------------------------------------------------- rm

PROTECTED_DIRS = {
    "/", "/bin", "/boot", "/dev", "/etc", "/home", "/lib", "/lib64", "/mnt", "/opt",
    "/proc", "/root", "/run", "/sbin", "/srv", "/sys", "/tmp", "/usr", "/var",
    HOME,
} | {
    os.path.join(HOME, d)
    for d in (".ssh", ".gnupg", ".dotfiles", ".claude", ".config", ".local", ".cache",
              ".passage", "Documents", "src", "Music", "Pictures", "Videos", "Downloads")
}
RM_CMDS = {"rm", "rmdir", "unlink", "shred"}
RM_ADVICE = (
    "Write each path out in full, e.g. `rm -rf /home/you/project/build`. "
    "No variables, globs, loops or relative paths."
)


def rm_paths(name, args):
    paths, i, opts_done = [], 0, False
    while i < len(args):
        a = args[i]
        if not opts_done and a == "--":
            opts_done = True
        elif not opts_done and a.startswith("-") and a != "-":
            if name == "shred" and a in ("-n", "-s", "--iterations", "--size", "--random-source"):
                i += 1
        else:
            paths.append(a)
        i += 1
    return paths


def check_rm(name, args, from_stdin):
    if name == "find":
        if "-delete" in args:
            deny(f"`find -delete` deletes paths computed at run time. List the matches first, then delete them by literal path. {RM_ADVICE}")
        for flag in ("-exec", "-execdir", "-ok", "-okdir"):
            if flag in args and args.index(flag) + 1 < len(args):
                if os.path.basename(args[args.index(flag) + 1]) in RM_CMDS:
                    deny(f"`find {flag} rm` deletes paths computed at run time. List the matches first, then delete them by literal path. {RM_ADVICE}")
        return
    if name not in RM_CMDS:
        return
    if from_stdin:
        deny(f"`xargs {name}` deletes paths computed at run time. {RM_ADVICE}")
    for p in rm_paths(name, args):
        if dynamic(p):
            deny(f"`{name} {p}`: the path uses a variable or command substitution, so it cannot be checked before it runs. {RM_ADVICE}")
        if any(c in p for c in "*?[]{}"):
            deny(f"`{name} {p}`: globs and brace expansions are not allowed. {RM_ADVICE}")
        if not (p.startswith("/") or p.startswith("~/") or p == "~"):
            deny(f"`{name} {p}`: relative path. {RM_ADVICE}")
        if ".." in p.split("/"):
            deny(f"`{name} {p}`: `..` is not allowed in delete paths. {RM_ADVICE}")
        full = os.path.normpath(os.path.expanduser(p))
        if full in PROTECTED_DIRS:
            deny(f"`{name} {p}` would delete a top-level or protected directory ({full}). Ask the user to do it themselves.")


# ----------------------------------------------------------------- commit

GIT_VALUE_OPTS = {"-C", "-c", "--git-dir", "--work-tree", "--namespace", "--exec-path"}
NON_IMPERATIVE = {
    "added", "adds", "adding", "fixed", "fixes", "fixing", "updated", "updates", "updating",
    "removed", "removes", "removing", "changed", "changes", "changing", "implemented",
    "implements", "implementing", "refactored", "refactors", "refactoring", "created",
    "creates", "creating", "deleted", "deletes", "deleting", "moved", "moves", "moving",
    "renamed", "renames", "renaming", "improved", "improves", "improving", "made", "makes",
    "making", "bumped", "bumps", "bumping", "merged", "merges", "merging", "cleaned",
    "cleans", "cleaning", "replaced", "replaces", "replacing", "switched", "switches",
    "switching", "enabled", "enables", "enabling", "disabled", "disables", "disabling",
}
TICKET_PREFIX_RE = re.compile(r"^(#?\d+|[A-Z][A-Z0-9]*-\d+):\s+")
TRAILER_RE = re.compile(r"^[A-Za-z][A-Za-z-]*: ")


def git_subcommand(args):
    i = 0
    while i < len(args):
        a = args[i]
        if a in GIT_VALUE_OPTS:
            i += 2
        elif a.startswith("-"):
            i += 1
        else:
            return a, args[i + 1:]
    return None, []


def commit_message(args, heredocs, cwd):
    """The commit message, or None when it is not given or not checkable.
    heredocs maps this command's heredoc placeholder ids to their bodies."""
    msgs, file_arg, i = [], None, 0

    def value(rest, idx):
        return (rest, idx) if rest else (args[idx + 1] if idx + 1 < len(args) else "", idx + 1)

    while i < len(args):
        a = args[i]
        if a == "--":
            break
        if a in ("--no-edit", "--fixup", "--squash", "--reuse-message", "--reedit-message") or a.startswith(
            ("--fixup=", "--squash=", "--reuse-message=", "--reedit-message=")
        ):
            return None
        if a.startswith("--message="):
            msgs.append(a.split("=", 1)[1])
        elif a == "--message":
            v, i = value("", i)
            msgs.append(v)
        elif a.startswith("--file="):
            file_arg = a.split("=", 1)[1]
        elif a == "--file":
            file_arg, i = value("", i)
        elif a.startswith("-") and not a.startswith("--") and len(a) > 1:
            flags = a[1:]
            if flags[0] in "Cc":
                return None  # -C/-c reuse another commit's message
            for k, f in enumerate(flags):
                if f == "m":
                    v, i = value(flags[k + 1:], i)
                    msgs.append(v)
                    break
                if f == "F":
                    file_arg, i = value(flags[k + 1:], i)
                    break
        i += 1

    if msgs:
        out = []
        for m in msgs:
            ref = PLACEHOLDER_RE.search(m)
            if ref:
                out.append(heredocs.get(int(ref.group(1)), ""))
            elif dynamic(m):
                return None
            else:
                out.append(m)
        return "\n\n".join(out)
    if file_arg == "-":
        return next(iter(heredocs.values()), None)
    if file_arg and not dynamic(file_arg):
        try:
            return Path(cwd, os.path.expanduser(file_arg)).read_text()
        except OSError:
            return None
    return None


def commit_problems(msg):
    lines = msg.rstrip("\n").split("\n")
    while lines and not lines[-1].strip():
        lines.pop()
    problems = []
    title = lines[0] if lines else ""
    if not title.strip():
        return ["the title is empty"]
    if title != title.strip():
        problems.append("the title has leading or trailing whitespace")
    if len(title) > 72:
        problems.append(f"the title is {len(title)} characters; the hard max is 72 (aim for 50)")
    if title.rstrip().endswith("."):
        problems.append("the title ends with a period")
    summary = TICKET_PREFIX_RE.sub("", title, count=1)
    first = summary.split()[0].lower() if summary.split() else ""
    if first in NON_IMPERATIVE:
        problems.append(f"the title starts with '{summary.split()[0]}'; use the imperative mood (e.g. 'Add', 'Fix')")

    if len(lines) < 2:
        problems.append("there is no body; add a blank line, then a `*` bulleted list of the changes")
        return problems
    if lines[1].strip():
        problems.append("line 2 must be blank (between title and body)")
    body = lines[2:]
    content = [l for l in body if l.strip()]
    if not content:
        problems.append("there is no body; add a `*` bulleted list of the changes")
        return problems
    for n, l in enumerate(body, start=3):
        if len(l) > 72:
            problems.append(f"line {n} is {len(l)} characters; wrap the body at 72")
    trailers_from = len(content)
    while trailers_from > 0 and TRAILER_RE.match(content[trailers_from - 1]):
        trailers_from -= 1
    bullets = content[:trailers_from]
    if bullets and not bullets[0].startswith("* "):
        problems.append("the body must be a bulleted list starting with `* `")
    for l in bullets:
        if not (l.startswith("* ") or l.startswith("  ")):
            problems.append(f"body line '{l[:40]}' is neither a `* ` bullet nor an indented continuation")
            break
    return problems


def check_commit(name, args, heredocs, cwd):
    if name != "git":
        return
    sub, rest = git_subcommand(args)
    if sub != "commit":
        return
    msg = commit_message(rest, heredocs, cwd)
    if msg is None:
        return
    problems = commit_problems(msg)
    if problems:
        deny(
            "The commit message breaks the format in ~/.claude/CLAUDE.md: "
            + "; ".join(problems)
            + ". Expected:\n\n<ticket>: Imperative summary under 50 chars\n\n* change one\n* change two\n"
        )


# --------------------------------------------------------------- outbound

NET_CMDS = {"curl", "wget", "http", "https", "xh", "aria2c", "lynx", "w3m", "nc", "ncat",
            "telnet", "sendmail", "mail", "mutt", "msmtp"}
BROWSER_TOOL_RE = re.compile(r"^mcp__.*(playwright|chrome|Browser)", re.IGNORECASE)


def identifiers():
    try:
        lines = Path(IDENTIFIERS_FILE).read_text().splitlines()
    except OSError:
        return []
    return [l.strip().lower() for l in lines if l.strip() and not l.strip().startswith("#")]


def leaked(text):
    if not text:
        return None
    hay = text.lower()
    decoded = unquote_plus(text).lower()
    for ident in identifiers():
        if ident in hay or ident in decoded:
            return ident
    return None


def check_outbound(tool, tool_input, commands):
    if tool == "Bash":
        cmd = tool_input.get("command", "")
        if not (any(c[0] in NET_CMDS for c in commands) or re.search(r"https?://", cmd)):
            return
        text = cmd
    elif tool == "WebFetch":
        text = tool_input.get("url", "")
    elif tool == "WebSearch":
        text = tool_input.get("query", "")
    elif BROWSER_TOOL_RE.match(tool):
        text = json.dumps(tool_input)
    else:
        return
    hit = leaked(text)
    if hit:
        raise Decision(
            "ask",
            f"This {tool} call sends one of the user's personal identifiers ({hit}) to an "
            "external service. The user must approve it explicitly; never fill in their "
            "contact details on your own.",
        )


# ------------------------------------------------------------------- main

DANGER_RE = re.compile(r"\b(kill|pkill|killall|rm|rmdir|unlink|shred|xargs|find|shutdown|reboot|poweroff|systemctl|loginctl)\b")


def check(tool, tool_input, cwd):
    commands = []
    if tool == "Bash":
        cmd = tool_input.get("command", "")
        try:
            commands = list(walk(cmd))
        except ValueError as e:
            if DANGER_RE.search(cmd):
                deny(f"The guard could not parse this command ({e}), and it contains a destructive command. Simplify the quoting and retry.")
            commands = []
        for name, args, from_stdin, heredocs in commands:
            check_kill(name, args, from_stdin)
            check_rm(name, args, from_stdin)
            check_commit(name, args, heredocs, cwd)
    check_outbound(tool, tool_input, commands)


def main():
    data = json.load(sys.stdin)
    tool = data.get("tool_name", "")
    tool_input = data.get("tool_input") or {}
    cwd = data.get("cwd") or os.getcwd()
    try:
        check(tool, tool_input, cwd)
    except Decision as d:
        print(json.dumps({
            "hookSpecificOutput": {
                "hookEventName": "PreToolUse",
                "permissionDecision": d.kind,
                "permissionDecisionReason": d.reason,
            }
        }))


if __name__ == "__main__":
    try:
        main()
    except Exception as e:  # never block a call because the guard itself broke
        print(f"guard.py internal error (call allowed): {e!r}", file=sys.stderr)
        sys.exit(1)
