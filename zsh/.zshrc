
export ZSH="$HOME/.oh-my-zsh"

plugins=(
    git # https://github.com/robbyrussell/oh-my-zsh/wiki/Plugin:git
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

eval "$(starship init zsh)"
