# if connecting with tramp from emacs, just return
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

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

#[[ -n "${key[Up]}" ]] && bindkey "${key[Up]}" up-line-or-beginning-search
#[[ -n "${key[Down]}" ]] && bindkey "${key[Down]}" down-line-or-beginning-search

bindkey "^[[A" up-line-or-beginning-search
bindkey "^[[B" down-line-or-beginning-search

# ssh
[ -f ~/bin/agent ] && source ~/bin/agent

# aliases
function ec() { emacsclient -c -nw "$@"; }
alias ed='emacs --daemon'
alias et='emacs --no-window-system'
alias vim='nvim'
alias rw='yarn redwood'

## docker
function docker-enter() { sudo docker exec -it "$@" /bin/bash; }
alias sdocker='sudo docker'
alias sdocker-compose='sudo docker-compose'
alias dcdestroy='sudo docker-compose stop && sudo docker-compose rm -f'
alias dclogs='sudo docker-compose logs -f'
alias dcps='sudo docker-compose ps'
alias dcstop='sudo docker-compose stop'
alias dcup='sudo docker-compose up -d'
alias docker-ip='sudo docker inspect -f "{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}"'
alias docker-rm-stopped='sudo docker system prune'
alias docker-rm-untagged='sudo docker images -q --filter "dangling=true" | xargs sudo docker rmi'

# git
alias gb='git branch'
alias gcb='git checkout -b'
alias gd='git diff'
alias glog='git log --oneline --decorate --graph'
alias gst='git status'
alias config='git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

# cli replacements
alias cat='${HOME}/.cargo/bin/bat'
alias ls='${HOME}/.cargo/bin/exa --git'

# fzf
# pop-os
[ -f /usr/share/doc/fzf/examples/key-bindings.zsh ] && source /usr/share/doc/fzf/examples/key-bindings.zsh
[ -f /usr/share/doc/fzf/examples/completion.zsh ] && source /usr/share/doc/fzf/examples/completion.zsh
# arch
[ -f /usr/share/fzf/key-bindings.zsh ] && source /usr/share/fzf/key-bindings.zsh
[ -f /usr/share/fzf/completion.zsh ] && source /usr/share/fzf/completion.zsh

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


export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
