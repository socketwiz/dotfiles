#!/usr/bin/env bash

set -euo pipefail

echo "Installing LSP servers..."

# Platform-specific packages
if [[ "$(uname)" == "Darwin" ]]; then
  brew install lua-language-server ruff rustup
  rustup install stable
  rustup component add rust-analyzer rust-src
else
  sudo pacman -S --needed --noconfirm \
    lua-language-server \
    python-ruff
fi

# Bash
npm install -g bash-language-server

# JavaScript / TypeScript
npm install -g \
  typescript \
  typescript-language-server \
  prettier \
  prettier-eslint-cli \
  vscode-langservers-extracted \
  @github/copilot-language-server \
  react-devtools

echo "LSP install complete"
