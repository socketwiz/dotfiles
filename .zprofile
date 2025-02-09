
# Ensure rtx is initialized
if command -v rtx >/dev/null 2>&1; then
    eval "$(rtx activate zsh)"
fi

# Ensure Node.js is found dynamically
if command -v node >/dev/null 2>&1; then
    export NODE_PATH="$(npm root -g)"
    export PATH="$(dirname "$(command -v node)"):$PATH"
fi

# setup secrets that don't belong in the repo
[ -f "$HOME/.secrets" ] && source "$HOME/.secrets"

