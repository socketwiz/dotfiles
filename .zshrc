# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
ZSH_THEME="rickyn"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git node brew npm urltools virtualenv, docker)

# add any zsh script fixes to bin in $HOME directory
source $ZSH/oh-my-zsh.sh
 
# PS1="(%n@%m) $PS1"

PAGER='less -x4 -X'

# This is L33T, it takes care of the screen 
# redraw issues when using less or man.
alias more='less -x4 -X'
alias emacs='emacs -nw'

# fix zsh globbing on some commands
alias gpg='nocorrect gpg'
alias rspec='nocorrect rspec'
alias hg='nocorrect hg'

alias glsf='git ls-files . -co --exclude-standard'

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

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
source ~/.fzf.zsh

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

## for docker
eval $(boot2docker shellinit 2>/dev/null)
alias docker-rm-stopped='docker rm $(docker ps -a -q)'
alias docker-rm-untagged='docker images -q --filter "dangling=true" | xargs docker rmi'
docker-enter() {
  boot2docker ssh '[ -f /var/lib/boot2docker/nsenter ] || docker run --rm -v /var/lib/boot2docker/:/target jpetazzo/nsenter'
  boot2docker ssh -t sudo /var/lib/boot2docker/docker-enter "$@"
}

## Go
export GOPATH=$HOME/go

## manta
source ~/.manta
