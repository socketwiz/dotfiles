# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
  git
  urltools
)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
export EDITOR='vim'

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
source ~/bin/agent

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# aliases
alias ec='emacsclient -c -n $1'
alias emacs='emacs -nw'

# fix zsh globbing on some commands
alias rspec='nocorrect rspec'
alias hg='nocorrect hg'

alias devbox='VBoxManage startvm DevBox --type headless'

## for docker
alias docker-rm-stopped='docker rm $(docker ps -a -q)'
alias docker-rm-untagged='docker images -q --filter "dangling=true" | xargs docker rmi'
function docker-enter() { docker exec -it "$@" /bin/bash; }

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

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

# setup Rust
export PATH=$HOME/.cargo/bin:$PATH

