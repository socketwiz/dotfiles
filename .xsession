#! /bin/sh

[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap
xrdb -merge ~/.Xresources &
xrandr --output DVI-I-1 --pos 0x0 --output DP-1 --pos 1920x0 --output DVI-D-0 --pos 3840x0 &
feh --bg-center ~/Pictures/2018-golf-r-blue.jpg

dbus-update-activation-environment --systemd DISPLAY
eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets)
export SSH_AUTH_SOCK

