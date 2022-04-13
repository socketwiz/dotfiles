### Shell configuration
#
export PAGER='less -x4 -XFR'

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
export EDITOR='nvim'

# enable command auto-correction.
export ENABLE_CORRECTION="true"

### Paths
#
# Add scripts I've written to the PATH.
export PATH=${HOME}/bin:${PATH}

# setup Rust
export PATH=${HOME}/.cargo/bin:${PATH}
source $HOME/.cargo/env

# setup Node
export PATH="${HOME}/.npm-global/bin":${PATH}

# Fixing terminal emacs copy-paste
# https://blog.d46.us/zsh-tmux-emacs-copy-paste/
function is-linux() { [[ "${OS_TYPE}" == "Linux" ]]; }
function is-darwin() { [[ "${OS_TYPE}" == "Darwin" ]]; }
function is-macos() { [[ "${OS_TYPE}" == "Darwin" ]]; }
