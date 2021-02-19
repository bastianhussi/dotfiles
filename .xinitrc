#!/usr/bin/env sh

xrdb -merge ~/.Xresources
setxkbmap de nodeadkeys -option caps:escape

numlockx

# Lock the screen after 10mins
xset s on
xset s 600
xss-lock -- lock &

~/.fehbg

picom -b &
nextcloud --background &
keepassxc &

exec dwm
