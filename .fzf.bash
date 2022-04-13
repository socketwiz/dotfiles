# Setup fzf
# ---------
if [[ ! "$PATH" == */home/hackerzol/.fzf/bin* ]]; then
  export PATH="${PATH:+${PATH}:}/home/hackerzol/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/hackerzol/.fzf/shell/completion.bash" 2> /dev/null

# Key bindings
# ------------
source "/home/hackerzol/.fzf/shell/key-bindings.bash"
