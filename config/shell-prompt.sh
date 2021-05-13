# Show only current directory name (not full path) on bash prompt
# ref: https://superuser.com/questions/60555/show-only-current-directory-name-not-full-path-on-bash-prompt
PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]\$ '
