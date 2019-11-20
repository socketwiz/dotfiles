# Set up the prompt

eval "$(starship init zsh)"

setopt histignorealldups sharehistory

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

# Use modern completion system
autoload -Uz compinit && compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# ssh
source ~/bin/agent

# aliases
alias ec='emacsclient -c -n $1'
alias ed='emacs --daemon'
alias vim='nvim'

## docker
alias docker-rm-stopped='docker system prune'
alias docker-rm-untagged='docker images -q --filter "dangling=true" | xargs docker rmi'
alias docker-ip='docker inspect -f "{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}"'
function docker-enter() { docker exec -it "$@" /bin/bash; }
alias dcdestroy='docker-compose stop && docker-compose rm -f'
alias dclogs='docker-compose logs -f'
alias dcup='docker-compose up -d'
alias dcstop='docker-compose stop'
alias dcps='docker-compose ps'

# git
alias gb='git branch'
alias gst='git status'
alias gd='git diff'
alias glog='git log --oneline --decorate --graph'


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

# present a list of tmux sessions to choose from
function tm() {
    select sel in $(tmux ls -F '#S'); do
        break;
    done
    tmux attach -t "$sel"
}
