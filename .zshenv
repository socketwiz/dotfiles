# Ensure base system paths are present
export PATH="/usr/local/bin:/usr/bin:/bin:$PATH"

### Shell configuration
export SHELL="$(command -v zsh)"
export PAGER='less -x4 -XFR'
export LANG=en_US.UTF-8
export EDITOR='nvim'

### Append custom tool paths
export DENO_INSTALL="${HOME}/.deno"
export PATH="$DENO_INSTALL/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.local/share/bob/nvim-bin:$PATH"

### Platform-specific environment
if [ "$(uname)" = "Darwin" ]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
  export WORKON_HOME="$HOME/.virtualenvs"
  export PATH="$HOME/bin:$HOME/.npm-packages/bin:$PATH"
  export PATH="$HOME/Library/Python/3.9/bin:$PATH"
  export PATH="/usr/local/opt/imagemagick@6/bin:$PATH"
  export PATH="/usr/local/opt/libxml2/bin:$PATH"
fi

if [ "$(uname)" = "Linux" ]; then
  export TMPDIR="$HOME/tmp"
  export QW_CONFIG="$HOME/.config/quickwit/quickwit.yaml"
  [ -d "$HOME/.pyenv/bin" ] && export PATH="$HOME/.pyenv/bin:$PATH"
fi

### pyenv PATH setup (shell integration in .zshrc)
if command -v pyenv >/dev/null 2>&1; then
  eval "$(pyenv init --path)"
fi
