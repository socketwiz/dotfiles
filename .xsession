#! /bin/sh

[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap
xrdb -merge ~/.Xresources &
xrandr --output DVI-I-1 --pos 0x0 --output DP-1 --pos 1920x0 --output DVI-D-0 --pos 3840x0 &
feh --bg-center ~/Pictures/ferrari.jpg

exec i3
