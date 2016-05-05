i3
# terminate things on logout
emacsclient --eval '(kill-emacs)'
gpg-connect-agent killagent /bye
pulseaudio --kill
