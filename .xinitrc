#!/usr/bin/env sh

eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

xrdb -merge ~/.Xresources

setxkbmap us -option ctrl:nocaps

# Lock the screen after 10mins
xset s on
xset s 600
# 'lock' is a wrapper-script around i3lock-color
xss-lock -- lock &

picom -b &
~/.fehbg &

/usr/lib/geoclue-2.0/demos/agent &
redshift &

emacs --daemon


for ENTRY in `ls -1 ${XDG_CONFIG_HOME:-$HOME/.config}/autostart`; do
   command gtk-launch $ENTRY
done &


while true; do
   xsetroot -name "$( date +"%F %R" )"
   sleep 1m  # Update time every minute
done &


session=${1:-dmw}

case $session in
    i3|i3wm    ) exec i3;;
    dwm        ) exec dwm;;
    xfce|xfce4 ) exec startxfce4;;
    # No known session, try to run it as command
    *          ) exec $1;;
esac
