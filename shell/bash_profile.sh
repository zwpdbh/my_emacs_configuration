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
# OPAM configuration
$HOME/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

# Something with Emacs terminal
alias e='emacsclient -t -a ""'
alias E="SUDO_EDITOR=\"emacsclient -t -a emacs\" sudoedit"

# GO settings 
export GO111MODULE=auto
export GOPATH=$HOME/go
export GOBIN=$GOPATH/bin
export PATH=$PATH:${GOPATH//://bin:}/bin
export PATH=$PATH:/usr/local/go/bin

# (Optional) for exercism completion.
if [ -f ~/.config/exercism/exercism_completion.bash ]; then
    source ~/.config/exercism/exercism_completion.bash
fi

# For direnv which augments existing shells with a new feature that can load and unload environment variables
# Install by: sudo apt install direnv, only support unix like system
eval "$(direnv hook bash)"
# For zsh: eval "$(direnv hook zsh)"


# get rid of "warning: GREP_OPTIONS is deprecated" message
unset GREP_OPTIONS