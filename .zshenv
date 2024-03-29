### Shell configuration
#
export PAGER='less -x4 -XFR'

# language environment
export LANG=en_US.UTF-8

# preferred editor for local and remote sessions
export EDITOR='nvim'

# enable command auto-correction.
export ENABLE_CORRECTION="true"

# local npm packages
export NPM_PACKAGES="$HOME/.npm-packages"
export NODE_PATH="$NPM_PACKAGES/lib/node_modules:$NODE_PATH"

### Paths
export PATH="$HOME/bin:${PATH}"
export PATH="$HOME/flutter/bin:$PATH"
export PATH="$DENO_INSTALL/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$NPM_PACKAGES/bin:$PATH"
export PATH="$HOME/.local/share/bob/nvim-bin:~/.local/share/rtx/shims:$PATH"

