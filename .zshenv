# Ensure base system paths are present
export PATH="/usr/local/bin:/usr/bin:/bin:$PATH"

### Shell configuration
export PAGER='less -x4 -XFR'
export LANG=en_US.UTF-8
export EDITOR='nvim'
export ENABLE_CORRECTION="true"

### Append custom tool paths
export PATH="$DENO_INSTALL/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.local/share/bob/nvim-bin:$PATH"
export PATH="$HOME/Library/Python/3.9/bin:$PATH"

if [ "$(uname)" = "Darwin" ]; then
  # Homebrew setup (adds its own entries, including /opt/homebrew/bin)
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi
