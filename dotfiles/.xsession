xsettingsd &
pa-applet &
nm-applet &
emacs --daemon &
dunst &
xss-lock lock &
~/session/scripts/displays &
xsetroot -solid gray10 -cursor_name left_ptr &
setxkbmap &
xmonad
# terminate things on logout
emacsclient --eval '(kill-emacs)'
gpg-connect-agent killagent /bye
pulseaudio --kill
