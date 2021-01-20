# Set up the prompt
eval "$(starship init zsh)"

setopt histignorealldups sharehistory

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e
# Edit command in vim with <ctrl-x ctrl-e>
autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

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
if whence dircolors >/dev/null; then
  eval "$(dircolors -b)"
  zstyle ':completion:*:default' list-colors "${(s.:.)LS_COLORS}"
else
  export CLICOLOR=1
  zstyle ':completion:*:default' list-colors ''
fi
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command "ps -u $USER -o pid,%cpu,tty,cputime,cmd"

# history (up-arrow completion)
autoload up-line-or-beginning-search
autoload down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

[[ -n "${key[Up]}" ]] && bindkey "${key[Up]}" up-line-or-beginning-search
[[ -n "${key[Down]}" ]] && bindkey "${key[Down]}" down-line-or-beginning-search

# ssh
source ~/bin/agent

# aliases
alias ed='emacs --daemon'
alias vim='nvim'
function find-commits() {
  find . -type d -name '.git' | while read -r dir ;
    do sh -c "cd $dir/../ && echo \"\nGIT STATUS IN ${dir//\.git/}\" && git status -s" ;
  done
}

## docker
function docker-enter() { sudo docker exec -it "$@" /bin/bash; }
alias docker='sudo docker'
alias docker-compose='sudo docker-compose'
alias dcdestroy='docker-compose stop && sudo docker-compose rm -f'
alias dclogs='docker-compose logs -f'
alias dcps='docker-compose ps'
alias dcstop='docker-compose stop'
alias dcup='docker-compose up -d'
alias docker-ip='docker inspect -f "{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}"'
alias docker-rm-stopped='docker system prune'
alias docker-rm-untagged='docker images -q --filter "dangling=true" | xargs sudo docker rmi'

# git
alias gb='git branch'
alias gcb='git checkout -b'
alias gd='git diff'
alias glog='git log --oneline --decorate --graph'
alias gst='git status'
alias config='git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# fix the friggin del key
bindkey "^[[3~" delete-char

case $(uname) in
  Darwin)
    source "$HOME/.darwin"
    ;;
  SunOS)
    source "$HOME/.sun"
    ;;
  Linux)
    source "$HOME/.linux"
    ;;
esac

# present a list of tmux sessions to choose from
function tm() {
    select sel in $(tmux ls -F '#S'); do
        break;
    done

    if [ -z "$sel" ]
    then
        echo "You didn't select an appropriate choice"
    else
        tmux attach -t "$sel"
    fi
}

export PATH="/usr/local/opt/node@12/bin:$PATH"
export PATH="/usr/local/opt/openjdk/bin:$PATH"
