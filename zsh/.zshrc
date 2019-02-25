# if [ -z "$TMUX" ] # When zsh is started attach to current tmux session or create a new one
# then
#     tmux attach -t TMUX || tmux new -s TMUX
# fi

export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="robbyrussell" # Set theme

plugins=(
    git # https://github.com/robbyrussell/oh-my-zsh/wiki/Plugin:git
    history-substring-search # ZSH port of Fish history search. Begin typing command, use up arrow to select previous use
    zsh-autosuggestions # Suggests commands based on your history
    zsh-completions # More completions
    zsh-syntax-highlighting # Fish shell like syntax highlighting for Zsh
    colored-man-pages # Self-explanatory
)
fpath+=~/.zfunc

autoload -U compinit && compinit # reload completions for zsh-completions

source $ZSH/oh-my-zsh.sh # required
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=5'

# This speeds up pasting w/ autosuggest
# https://github.com/zsh-users/zsh-autosuggestions/issues/238
pasteinit() {
    OLD_SELF_INSERT=${${(s.:.)widgets[self-insert]}[2,3]}
    zle -N self-insert url-quote-magic # I wonder if you'd need `.url-quote-magic`?
}

pastefinish() {
    zle -N self-insert $OLD_SELF_INSERT
}
zstyle :bracketed-paste-magic paste-init pasteinit
zstyle :bracketed-paste-magic paste-finish pastefinish

source ~/dotfiles/zsh/.zshrc.noobs

# Set Spaceship ZSH as a prompt
autoload -U promptinit; promptinit
prompt spaceship

export SPACESHIP_PACKAGE_SHOW=false

export PATH="/usr/local/opt/texinfo/bin:$PATH"
