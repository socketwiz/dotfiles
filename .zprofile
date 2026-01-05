# setup secrets that don't belong in the repo
[ -f "$HOME/.secrets" ] && source "$HOME/.secrets"

# Ensure homebrew binaries (including bash 5) are found first
# Must be here (after /etc/zprofile's path_helper) not in .zshenv
export PATH="/opt/homebrew/bin:/opt/homebrew/sbin:$PATH"
