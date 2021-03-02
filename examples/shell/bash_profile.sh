alias ll="ls -la"
alias ls='ls --color'
alias emacs='emacs-27.1.exe'
##############
### Basics ###
##############
 
# Don't wait for job termination notification
set -o notify
 
# Enables UTF-8 in Putty.
# See http://www.earth.li/~huggie/blog/tech/mobile/putty-utf-8-trick.html
echo -ne '\e%G\e[?47h\e%G\e[?47l'
 
# My pretty prompt (Yay!)
PS1='\[\e[32;40m\]\u\[\e[0m\]\[\e[34;40m\]\H\[\e[0m\]\[\e[40;1m\]\w\[\e[0m\] '
 
# Start SSH Agent
#----------------------------

SSH_ENV="$HOME/.ssh/environment"

function run_ssh_env {
  . "${SSH_ENV}" > /dev/null
}

function start_ssh_agent {
  echo "Initializing new SSH agent..."
  ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
  echo "succeeded"
  chmod 600 "${SSH_ENV}"

  run_ssh_env;

  ssh-add ~/.ssh/id_rsa;
}

if [ -f "${SSH_ENV}" ]; then
  run_ssh_env;
  ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
    start_ssh_agent;
  }
else
  start_ssh_agent;
fi
####################################