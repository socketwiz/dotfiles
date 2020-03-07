#! /usr/bin/env bash

# set caps lock to control
[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap
# xrdb -merge ~/.Xresources &

# setup the 3 monitors
xrandr --output DP-4 --pos 0x0 --output DP-2 --pos 5760x0 --output DP-0 --pos 11520x0 &

# desktop background image
feh --bg-center ~/Pictures/ferrari.jpg

# setup keyring for storing secrets and making them available to applications
dbus-update-activation-environment --systemd DISPLAY
eval "$(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets)"
export SSH_AUTH_SOCK

