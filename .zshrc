# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
ZSH_THEME="rickyn"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git node brew npm urltools docker)

# add any zsh script fixes to bin in $HOME directory
source $ZSH/oh-my-zsh.sh
 
PAGER='less -x4 -X'

setopt CORRECT

# This is L33T, it takes care of the screen 
# redraw issues when using less or man.
alias more='less -x4 -X'

# fix zsh globbing on some commands
alias rspec='nocorrect rspec'
alias hg='nocorrect hg'

# git
alias glsf='git ls-files . -co --exclude-standard'

alias devbox='VBoxManage startvm DevBox --type headless'

# print a list of shell functions
alias lsfuncs="print -l ${(ok)functions}"

# fix the friggin del key
bindkey "^[[3~" delete-char

case `uname` in
  Darwin)
    source $HOME/.darwin
    ;;
  SunOS)
    source $HOME/.sun
    ;;
  Linux)
    source $HOME/.linux
    ;;
  CYGWIN_NT-10.0-WOW)
    source $HOME/.windows
    ;;
esac

## for docker
alias docker-rm-stopped='docker rm $(docker ps -a -q)'
alias docker-rm-untagged='docker images -q --filter "dangling=true" | xargs docker rmi'
function docker-enter() { docker exec -it "$@" /bin/bash; }

## manta
source ~/.manta

# Setup user agent
env=~/.ssh/agent.env

agent_load_env () {
    test -f "$env" && . "$env" >| /dev/null;
}

agent_start () {
    (umask 077; ssh-agent >| "$env")
    . "$env" >| /dev/null;
}

agent_load_env

# agent_run_state: 0=agent running w/ key; 1=agent w/o key; 2= agent not running
agent_run_state=$(ssh-add -l >| /dev/null 2>&1; echo $?)

if [ ! "$SSH_AUTH_SOCK" ] || [ $agent_run_state = 2 ]; then
    agent_start
    ssh-add
elif [ "$SSH_AUTH_SOCK" ] && [ $agent_run_state = 1 ]; then
    ssh-add
fi

unset env

REACT_EDITOR=mvim

[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

