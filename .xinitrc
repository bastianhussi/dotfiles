#!/bin/sh

eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

eval $(ssh-agent)

xrdb -merge ~/.Xresources
setxkbmap -option caps:escape

picom -b &

# ~/.fehbg &

nextcloud --background &
keepassxc &

# Lock the screen after 10mins
xset s on
xset s 600
xss-lock -- lock &

exec qtile
