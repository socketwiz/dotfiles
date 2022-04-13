
export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="powerlevel10k/powerlevel10k" # Set theme
POWERLEVEL9K_MODE=nerdfont-complete
PURE_POWER_MODE=fancy

plugins=(
    git # https://github.com/robbyrussell/oh-my-zsh/wiki/Plugin:git
    #history-substring-search # ZSH port of Fish history search. Begin typing command, use up arrow to select previous use
    zsh-completions # More completions
    zsh-syntax-highlighting # Fish shell like syntax highlighting for Zsh
    colored-man-pages # Self-explanatory
)
fpath+=~/.zfunc

autoload -U compinit && compinit # reload completions for zsh-completions

source $ZSH/oh-my-zsh.sh # required
source ~/dotfiles/zsh/.zshrc.noobs

export PATH="/usr/local/opt/texinfo/bin:$PATH"
fpath=($fpath "/home/socketwiz/.zfunctions")

source ~/.purepower
