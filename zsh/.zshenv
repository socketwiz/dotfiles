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
# setup Homebrew
BREW_PATHS=/usr/local/bin:/usr/local/sbin
export PATH=${BREW_PATHS}:${PATH}

# Add scripts I've written to the PATH.
export PATH=${HOME}/bin:${PATH}

# setup Rust
export PATH=${HOME}/.cargo/bin:${PATH}

# setup Node
export PATH="${HOME}/.npm-global/bin":${PATH}

