#!/usr/bin/env python3
"""Tests for guard.py. Run: python3 -I ~/.claude/hooks/test_guard.py"""

import importlib.util
import json
import os
import shutil
import subprocess
import sys
import tempfile
import time
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
TMP = tempfile.mkdtemp(prefix="guard-test.")
IDS = Path(TMP, "ids")
IDS.write_text("# test identifiers\nme@example.test\nOther.Person@Example.Test\n")
os.environ["GUARD_IDENTIFIERS_FILE"] = str(IDS)

spec = importlib.util.spec_from_file_location("guard", HERE / "guard.py")
guard = importlib.util.module_from_spec(spec)
spec.loader.exec_module(guard)


def decide(cmd=None, tool="Bash", tool_input=None, cwd=TMP):
    """'allow', or 'deny: reason' / 'ask: reason'."""
    if tool_input is None:
        tool_input = {"command": cmd}
    try:
        guard.check(tool, tool_input, cwd)
    except guard.Decision as d:
        return f"{d.kind}: {d.reason}"
    return "allow"


class Base(unittest.TestCase):
    def allow(self, cmd=None, **kw):
        self.assertEqual(decide(cmd, **kw), "allow", cmd)

    def deny(self, cmd, fragment="", **kw):
        r = decide(cmd, **kw)
        self.assertTrue(r.startswith("deny: "), f"{cmd!r} -> {r}")
        self.assertIn(fragment, r)


class Parsing(Base):
    def test_split_and_unwrap(self):
        cmds = [c[:3] for c in guard.walk("FOO=1 sudo -u root timeout 5s kill 12 2>/dev/null && echo ok | xargs -n1 rm")]
        self.assertEqual(cmds, [("kill", ["12"], False), ("echo", ["ok"], False), ("rm", [], True)])

    def test_sh_c_and_eval_are_walked(self):
        names = [c[0] for c in guard.walk("bash -c 'cd /x; rm -rf build' && eval kill 5")]
        self.assertEqual(names, ["bash", "cd", "rm", "eval", "kill"])

    def test_heredoc_body_is_not_parsed_as_commands(self):
        cmd = "cat > /tmp/x <<'EOF'\nrm -rf $HOME\nit's fine\nEOF\necho done"
        self.assertEqual([c[0] for c in guard.walk(cmd)], ["cat", "echo"])

    def test_comment_does_not_hide_later_commands(self):
        self.deny("# it's a comment\nrm -rf build", "relative path")
        self.deny("cd /x # go there\nrm -rf build", "relative path")
        self.allow("echo '# not a comment'; echo ok")
        self.deny("echo a#b; rm -rf build", "relative path")

    def test_heredoc_marker_in_single_quotes_is_literal(self):
        self.allow("ssh host 'python3 - <<EOF\nprint(\"it is\")\nEOF'")
        self.deny("ssh host 'cat <<EOF\nx\nEOF'; rm -rf build", "relative path")

    def test_line_continuation(self):
        self.deny("rm -rf \\\n  build", "relative path")

    def test_unparsable_destructive_command_denied(self):
        self.deny("rm -rf '/tmp/x", "could not parse")

    def test_unparsable_harmless_command_allowed(self):
        self.allow("echo 'unterminated")


class Kill(Base):
    @classmethod
    def setUpClass(cls):
        cls.sleeper = subprocess.Popen(["sleep", "300"])
        fake = Path(TMP, "pipewire")
        shutil.copy(shutil.which("sleep"), fake)
        cls.fake = subprocess.Popen([str(fake), "300"])
        time.sleep(0.2)

    @classmethod
    def tearDownClass(cls):
        for p in (cls.sleeper, cls.fake):
            p.kill()
            p.wait()

    def test_plain_kill_of_own_child_allowed(self):
        self.allow(f"kill {self.sleeper.pid}")
        self.allow(f"kill -9 {self.sleeper.pid}")
        self.allow(f"kill -s TERM -- {self.sleeper.pid}")

    def test_kill_list_and_jobs_allowed(self):
        self.allow("kill -l")
        self.allow("kill %1")

    def test_nonexistent_pid_allowed(self):
        self.allow("kill 4194300")

    def test_pid_1(self):
        self.deny("kill 1", "PID 1")

    def test_ancestor_denied(self):
        # The incident: killing a parent that turned out to be the session.
        self.deny(f"kill -9 {os.getppid()}", "ancestor of this Claude session")

    def test_session_critical_comm_denied(self):
        self.deny(f"kill {self.fake.pid}", "(pipewire) is session-critical")

    def test_process_groups_denied(self):
        self.deny("kill -9 -1", "process group")
        self.deny("kill -- -1234", "process group")
        self.deny("kill 0", "process group")

    def test_computed_targets_denied(self):
        self.deny("kill $PPID", "computed target")
        self.deny("kill -9 $(pgrep foo)", "computed target")
        self.deny("pgrep foo | xargs kill", "xargs kill")
        self.deny(f"sudo kill {os.getppid()}", "ancestor")

    def test_other_users_process_denied(self):
        self.deny("kill 2", "")  # kthreadd: root-owned

    def test_pkill(self):
        token = f"guardtest{os.getpid()}"
        p = subprocess.Popen(["sleep", "301"], env={**os.environ}, )
        try:
            self.allow(f"pkill -f {token}")  # matches nothing
            self.allow(f"pkill -TERM -x sleep -P {os.getpid()}")
            self.deny("pkill -x pipewire", "session-critical")
            self.deny("pkill -9 -x systemd", "PID 1")
            self.deny("pkill -f $X", "computed target")
        finally:
            p.kill()
            p.wait()

    def test_killall(self):
        self.allow("killall no-such-process-xyz")
        self.deny("killall pipewire", "session-critical")
        self.deny("killall -u zolmok", "every process of that user")

    def test_session_enders(self):
        self.deny("systemctl --user exit", "ends the login session")
        self.deny("loginctl terminate-user zolmok", "ends the login session")
        self.deny("hyprctl dispatch exit", "ends the login session")
        self.deny("sudo reboot", "ends the login session")
        self.allow("systemctl --user restart pipewire.service")
        self.allow("systemctl status sshd")


class Rm(Base):
    def test_literal_absolute_paths_allowed(self):
        self.allow("rm -rf /home/zolmok/src/project/build")
        self.allow("rm -f ~/tmp/old.log /var/tmp/x")
        self.allow("command rm -rf '/home/zolmok/tmp/with space' </dev/null")
        self.allow("rmdir /home/zolmok/tmp/empty")

    def test_variables_and_substitution_denied(self):
        self.deny('rm -rf "$A/$top"', "variable")
        self.deny("rm -rf $(cat list)", "variable")
        self.deny("for f in a b; do rm /tmp/x/$f; done", "variable")
        self.deny("bash -c 'rm -rf $DIR'", "variable")
        self.deny("if true; then rm build; fi", "relative path")

    def test_relative_glob_dotdot_denied(self):
        self.deny("rm -rf build", "relative path")
        self.deny("rm ./x", "relative path")
        self.deny("rm /tmp/x/*.log", "globs")
        self.deny("rm /tmp/{a,b}", "globs")
        self.deny("rm -rf /home/zolmok/src/../Documents", "`..`")

    def test_protected_dirs_denied(self):
        self.deny("rm -rf /", "protected directory")
        self.deny("rm -rf ~", "protected directory")
        self.deny("rm -rf /home/zolmok/", "protected directory")
        self.deny("sudo rm -rf /usr", "protected directory")
        self.deny("rm -rf ~/.ssh", "protected directory")
        self.allow("rm -f ~/.ssh/known_hosts.old")

    def test_find_and_xargs_denied(self):
        self.deny("find /tmp/x -name '*.o' -delete", "find -delete")
        self.deny("find /tmp/x -exec rm {} +", "find -exec rm")
        self.deny("ls /tmp/x | xargs rm", "xargs rm")
        self.allow("find /tmp/x -name '*.o'")

    def test_rm_word_in_strings_is_not_rm(self):
        self.allow("echo 'rm -rf build'")
        self.allow("git commit -m 'Remove rm $X usage' --no-edit")


class Commit(Base):
    GOOD = "#12: Add config loader\n\n* Load JSON config with defaults\n* Validate the port range\n"

    def test_good_message_via_m(self):
        self.allow(f"git commit -m '{self.GOOD}'")

    def test_good_message_via_heredoc(self):
        cmd = f"git commit -m \"$(cat <<'EOF'\n{self.GOOD}EOF\n)\""
        self.allow(cmd)

    def test_heredoc_belongs_to_its_own_command(self):
        cmd = ("python3 - <<'EOF'\nprint('x')\nEOF\n"
               f"git commit -m \"$(cat <<'EOF'\n{self.GOOD}EOF\n)\"")
        self.allow(cmd)
        bad = ("cat > /tmp/f <<'EOF'\n" + self.GOOD + "EOF\n"
               "git commit -m \"$(cat <<'EOF'\nAdded it\nEOF\n)\"")
        self.deny(bad, "imperative")

    def test_good_message_via_F_stdin_and_multiple_m(self):
        self.allow(f"git commit -F - <<'EOF'\n{self.GOOD}EOF")
        self.allow("git commit -m 'Add loader' -m '* Load JSON config'")

    def test_good_message_via_file(self):
        Path(TMP, "msg").write_text(self.GOOD)
        self.allow("git commit -F msg")

    def test_no_ticket_prefix_ok(self):
        self.allow("git commit -m 'Add loader\n\n* Load JSON'")

    def test_title_problems(self):
        self.deny("git commit -m 'Add loader.\n\n* x'", "ends with a period")
        self.deny(f"git commit -m '{'A' * 73}\n\n* x'", "73 characters")
        self.deny("git commit -m '#12: Added loader\n\n* x'", "imperative")
        self.deny("git commit -m 'Fixes the bug\n\n* x'", "imperative")

    def test_body_problems(self):
        self.deny("git commit -m 'Add loader'", "no body")
        self.deny("git commit -am 'Add loader'", "no body")
        self.deny("git commit -m 'Add loader\n* x'", "line 2 must be blank")
        self.deny("git commit -m 'Add loader\n\nLoads the config.'", "bulleted list")
        self.deny(f"git commit -m 'Add loader\n\n* {'x' * 80}'", "wrap the body at 72")

    def test_continuations_and_trailers_ok(self):
        self.allow("git commit -m 'Add loader\n\n* Load JSON config with defaults so a\n  missing file is not fatal\n\nRefs: #12'")

    def test_unchecked_forms_pass(self):
        self.allow("git commit --amend --no-edit")
        self.allow("git commit --fixup HEAD~1")
        self.allow('git commit -m "$MSG"')
        self.allow("git commit")  # editor
        self.allow("git -C /tmp/x log -m")

    def test_global_options_before_commit(self):
        self.deny("git -C /tmp/x -c user.name=t commit -m 'Added it'", "imperative")


class Outbound(Base):
    def test_curl_with_identifier_asks(self):
        r = decide("curl -A 'bot me@example.test' https://sec.gov/x")
        self.assertTrue(r.startswith("ask: "), r)

    def test_case_and_url_encoding(self):
        self.assertTrue(decide("curl 'https://x.test/?c=other.person%40example.test'").startswith("ask: "))
        self.assertTrue(decide("wget https://x.test/?e=ME@EXAMPLE.TEST").startswith("ask: "))

    def test_webfetch_websearch_browser(self):
        self.assertTrue(decide(tool="WebFetch", tool_input={"url": "https://x.test/?e=me%40example.test", "prompt": "p"}).startswith("ask: "))
        self.assertTrue(decide(tool="WebSearch", tool_input={"query": "me@example.test"}).startswith("ask: "))
        self.assertTrue(decide(tool="mcp__playwright__browser_type", tool_input={"text": "me@example.test"}).startswith("ask: "))

    def test_clean_requests_allowed(self):
        self.allow("curl -s https://example.com")
        self.allow(tool="WebFetch", tool_input={"url": "https://example.com", "prompt": "summarize me@example.test"})
        self.allow(tool="mcp__ynab__list_accounts", tool_input={"note": "me@example.test"})

    def test_local_commands_with_identifier_allowed(self):
        self.allow("git log --author=me@example.test")

    def test_missing_identifiers_file_checks_nothing(self):
        old = guard.IDENTIFIERS_FILE
        guard.IDENTIFIERS_FILE = str(Path(TMP, "absent"))
        try:
            self.allow("curl https://x.test/?e=me@example.test")
        finally:
            guard.IDENTIFIERS_FILE = old


class EndToEnd(unittest.TestCase):
    def run_hook(self, payload):
        return subprocess.run(
            [sys.executable, "-I", str(HERE / "guard.py")],
            input=json.dumps(payload), capture_output=True, text=True, timeout=20,
        )

    def test_deny_json(self):
        r = self.run_hook({"tool_name": "Bash", "tool_input": {"command": "rm -rf build"}, "cwd": TMP})
        out = json.loads(r.stdout)["hookSpecificOutput"]
        self.assertEqual(out["permissionDecision"], "deny")
        self.assertEqual(out["hookEventName"], "PreToolUse")

    def test_allow_is_silent(self):
        r = self.run_hook({"tool_name": "Bash", "tool_input": {"command": "ls"}, "cwd": TMP})
        self.assertEqual((r.returncode, r.stdout), (0, ""))

    def test_internal_error_does_not_block(self):
        r = self.run_hook({"tool_name": "Bash", "tool_input": {"command": 42}, "cwd": TMP})
        self.assertEqual((r.returncode, r.stdout), (1, ""))
        self.assertIn("internal error", r.stderr)


if __name__ == "__main__":
    try:
        unittest.main(verbosity=1)
    finally:
        shutil.rmtree(TMP, ignore_errors=True)
