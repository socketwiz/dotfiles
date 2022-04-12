# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
ZSH_THEME="robbyrussell"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(rails git textmate ruby brew gem)

# add any zsh script fixes to bin in $HOME directory
PATH=$HOME/bin:$PATH
source $ZSH/oh-my-zsh.sh
 
# fix the dumb ssh-agent
SSH_ENV="$HOME/.ssh/environment"

function start_agent {
  echo "Initialising new SSH agent..."
  /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
  echo succeeded
  chmod 600 "${SSH_ENV}"
  . "${SSH_ENV}" > /dev/null
  /usr/bin/ssh-add $HOME/.aws/lsec2.pem;
}

# Source SSH settings, if applicable
if [ -f "${SSH_ENV}" ]; then
  . "${SSH_ENV}" > /dev/null
  ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
    start_agent;
  }
else
  start_agent;
fi

PS1="(%n@%m) $PS1"
SUDO_PS1="\[\e[33;1;41m\][\u] \w \$\[\e[0m\] "

PAGER='less -x4 -X'

# This is L33T, it takes care of the screen 
# redraw issues when using less or man.
alias more='less -x4 -X'
alias emacs='emacs -nw'

# fix zsh globbing on some commands
alias gpg='nocorrect gpg'
alias rspec='nocorrect rspec'
alias hg='nocorrect hg'

# fix the friggin del key
bindkey "^[[3~" delete-char

# web2.0 domain generator
domain_gen()
{
  for domain in $(pwgen -1A0B 6 10); do 
    echo -ne "$domain.com "; 
    if [ -z "$(whois $domain.com | grep -o 'No match for')" ]; then 
      echo -ne "Not "; 
    fi; 
    echo "Available for register"; 
  done  
}


case `uname` in
  Darwin)
    source $HOME/.darwin
    ;;
  SunOS)
    source $HOME/.sun
    ;;
esac
