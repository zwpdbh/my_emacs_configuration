# Show only current directory name (not full path) on bash prompt
# ref: https://superuser.com/questions/60555/show-only-current-directory-name-not-full-path-on-bash-prompt
PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]\$ '

alias ls="ls --color"
alias ll="ls -la"
alias cbuild="cmake -H. -Bbuild && cmake --build build -- -j4"

# For erlang rebar3 
export PATH=/home/zw/.cache/rebar3/bin:$PATH
export PATH=/usr/lib/erlang/lib/tools-2.11.1/emacs:$PATH
export PATH=/usr/lib/erlang:$PATH

# For Ocaml bins
export PATH=~/.opam/system/bin:$PATH
# For update Ocaml commands
eval $(opam config env)

# Something with Emacs terminal
alias e='emacsclient -t -a ""'
alias E="SUDO_EDITOR=\"emacsclient -t -a emacs\" sudoedit"


# GO settings 
export PATH=$PATH:/usr/local/go/bin
export GO111MODULE=auto
