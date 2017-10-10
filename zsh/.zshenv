
export PAGER='less -x4 -X'

# Homebrew paths
export BREW_PATHS=/usr/local/bin:/usr/local/sbin

export GOPATH="$HOME/go-workspace"
export PATH="$PATH:$GOPATH/bin"
export PATH=$HOME/.config/yarn/global/node_modules/.bin:/usr/local/bin:$HOME/bin:$PATH
### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
# Add scripts I've written to the PATH.
export PATH=$HOME/bin:$HOME/.aws/bin:$BREW_PATHS:${PATH}

# Node.js paths
export PATH=/usr/local/share/npm/bin:$PATH

