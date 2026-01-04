#!/usr/bin/env bash

set -euo pipefail

echo "Installing LSP servers..."

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

# Python
pip3 install --upgrade ruff-lsp

rustup install stable
rustup component add rust-analyzer rust-src

# Lua (via Homebrew)
brew install lua-language-server

echo "✅ LSP install complete"
