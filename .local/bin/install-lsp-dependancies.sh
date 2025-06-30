#! /usr/bin/env bash

#
# Bash
npm install -g bash-language-server
#
# Javascript / TypeScript
npm install -g prettier-eslint-cli
npm install -g prettier
npm install -g react-devtools
npm install -g vscode-langservers-extracted
npm install -g typescript typescript-language-server
npm install -g @github/copilot-language-server
#
# HTML
npm install -g vscode-html-languageserver-bin
#
# JSON
npm install -g vscode-json-languageserver
#
# Python
pip3 install python-lsp-server

#
# Rust
rustup component add rls rust-analysis rust-src
# Lua
brew install lua-language-server
