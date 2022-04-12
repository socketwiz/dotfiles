# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
ZSH_THEME="rickyn"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

PATH=$HOME/.config/yarn/global/node_modules/.bin:/usr/local/bin:$HOME/bin:$PATH

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git node brew npm urltools virtualenvwrapper docker)

# add any zsh script fixes to bin in $HOME directory
source $ZSH/oh-my-zsh.sh
 
PAGER='less -x4 -X'

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
esac

source ~/.fzf.zsh

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

## for docker
eval "$(docker-machine env default 2>/dev/null)"
alias docker-rm-stopped='docker rm $(docker ps -a -q)'
alias docker-rm-untagged='docker images -q --filter "dangling=true" | xargs docker rmi'
function docker-enter() { docker exec -it "$@" /bin/bash; }

## manta
source ~/.manta

# Predictable SSH authentication socket location.
SOCK="/tmp/ssh-agent-$USER-screen"
if test $SSH_AUTH_SOCK && [ $SSH_AUTH_SOCK != $SOCK ]
then
    rm -f /tmp/ssh-agent-$USER-screen
    ln -sf $SSH_AUTH_SOCK $SOCK
    export SSH_AUTH_SOCK=$SOCK
fi

[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"
export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

REACT_EDITOR=mvim
