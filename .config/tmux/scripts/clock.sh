#!/bin/sh
# Status-bar clock.
#
# This lives in a script rather than inline in tmux.conf on purpose. tmux runs
# strftime over the status string itself, including the text inside #(...), so
# an inline "#(date '+%l:%M %p')" never reaches date: tmux substitutes the %
# sequences first, using the zone the server cached from /etc/localtime at
# startup, and date is handed an already-rendered literal to echo back. After a
# timedatectl set-timezone that leaves the clock an hour off until the server is
# killed, and wrapping it in #() does not help.
#
# With no % in tmux.conf there is nothing for tmux to substitute, so this runs
# as written and a freshly forked date re-reads /etc/localtime every time.
# 24-hour, to match the system bar's "2026-08-28 14:43".
exec date '+%H:%M'
