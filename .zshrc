# if connecting with tramp from emacs, just return
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

# Set up the prompt
eval "$(starship init zsh)"

setopt histignorealldups sharehistory correct
setopt AUTO_CD AUTO_PUSHD PUSHD_IGNORE_DUPS  # directory navigation
setopt EXTENDED_GLOB                          # powerful globbing
setopt NO_BEEP                                # silence
setopt HIST_IGNORE_SPACE                      # don't save commands starting with space
setopt HIST_REDUCE_BLANKS                     # remove extra whitespace
setopt HIST_VERIFY                            # show expanded history before running
setopt INC_APPEND_HISTORY                     # write immediately, not on exit

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e
# Edit command in vim with <ctrl-x ctrl-e>
autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

# History configuration
HISTSIZE=50000
SAVEHIST=50000
HISTFILE=~/.zsh_history

# Completion system with daily cache rebuild
autoload -Uz compinit
if [[ -n ~/.zcompdump(#qN.mh+24) ]]; then
  compinit
else
  compinit -C
fi

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

bindkey "^[[A" up-line-or-beginning-search
bindkey "^[[B" down-line-or-beginning-search

# ssh
[ -f ~/bin/agent ] && source ~/bin/agent

# aliases
function ec() { emacsclient -c -nw "$@"; }
function git() {
  local git_bin="/usr/bin/git"
  local current_path="$(realpath "$PWD")"
  local home_real="$(realpath "$HOME")"

  # Use dotfiles repo when:
  # - Exactly in $HOME (not subdirectories)
  # - In $HOME/.config or its subdirectories
  # - In $HOME/.emacs.d or its subdirectories
  if [[ "$current_path" == "$home_real" ]]; then
    "$git_bin" --git-dir="$HOME/.dotfiles" --work-tree="$HOME" "$@"
    return
  fi

  for allowed in "$HOME/.config" "$HOME/.emacs.d"; do
    local allowed_real="$(realpath "$allowed" 2>/dev/null)" || continue
    if [[ "$current_path" == "$allowed_real" || "$current_path" == "$allowed_real"/* ]]; then
      "$git_bin" --git-dir="$HOME/.dotfiles" --work-tree="$HOME" "$@"
      return
    fi
  done

  "$git_bin" "$@"
}
alias ed='emacs --daemon'
alias et='emacs --no-window-system'
alias vim='nvim'
alias rw='source .dev.env; yarn redwood'

## docker
function docker-enter() { sudo docker exec -it "$@" /bin/bash; }
alias sdocker='sudo docker'
alias sdc='sudo docker compose'
alias dcdestroy='sudo docker compose stop && sudo docker compose rm -f'
alias dclogs='sudo docker compose logs -f'
alias dcps='sudo docker compose ps'
alias dcstop='sudo docker compose stop'
alias dcup='sudo docker compose up -d'
alias docker-ip='sudo docker inspect -f "{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}"'
alias docker-rm-stopped='sudo docker system prune'
alias docker-rm-untagged='sudo docker images -q --filter "dangling=true" | xargs sudo docker rmi'

# git
alias ga='git add'
alias gb='git branch'
alias gc='git commit'
alias gco='git checkout'
alias gcb='git checkout -b'
alias gd='git diff'
alias gl='git pull'
alias glog='git log --oneline --decorate --graph'
alias gp='git push'
alias gst='git status'
alias gsw='git switch'

# safety
alias rm='rm -I'
alias mv='mv -i'
alias cp='cp -i'

# cli replacements
alias cat='bat'
alias ls='eza --git'

# cargo build management
alias cargo-nice='nice -n 19 cargo'
alias cargo-low='nice -n 19 ionice -c 3 cargo'
alias cargo-single='cargo -j 1'

# command-line fuzzy finder
FZF_BIN="$(command -v fzf)"

if [ -n "$FZF_BIN" ]; then
    source <("$FZF_BIN" --zsh)
fi

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

# pyenv shell integration - lazy loaded (PATH setup is in .zshenv)
if command -v pyenv >/dev/null 2>&1; then
  pyenv() {
    unfunction pyenv
    eval "$(command pyenv init -)"
    pyenv "$@"
  }
fi

# mise setup
eval "$(mise activate zsh)"

# zoxide - smart directory jumping (use 'z' to jump)
if command -v zoxide >/dev/null 2>&1; then
  eval "$(zoxide init zsh)"
fi
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local
