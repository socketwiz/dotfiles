
export PAGER='less -x4 -XFR'

# Homebrew paths
export BREW_PATHS=/usr/local/bin:/usr/local/sbin

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
export EDITOR='nvim'

# enable command auto-correction.
export ENABLE_CORRECTION="true"

### Paths
#
# Add scripts I've written to the PATH.
export PATH=$HOME/bin:$HOME/.aws/bin:$BREW_PATHS:${PATH}

# setup Rust
export PATH=$HOME/.cargo/bin:$PATH

# setup go
export PATH="$PATH:$HOME/go-workspace/bin"

