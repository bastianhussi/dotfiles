export _JAVA_AWT_WM_NONREPARENTING=1

# Running Gdm on tty1 and Gnome on tty2 for the moment
if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -eq 3 ]; then
  exec startx
fi
