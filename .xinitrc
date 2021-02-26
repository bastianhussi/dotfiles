#!/usr/bin/env sh

eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK
eval $(ssh-agent)

xrdb -merge ~/.Xresources
setxkbmap de nodeadkeys -option caps:escape

# Activate the numblock keys
numlockx

# Lock the screen after 10mins
xset s on
xset s 600
# 'lock' is a wrapper-script around i3lock-color
xss-lock -- lock &

picom -b &
redshift &
rsblocks &
~/.fehbg &

for ENTRY in `ls -1 ${XDG_CONFIG_HOME:-$HOME/.config}/autostart`; do
   command gtk-launch $ENTRY
done

exec dwm
