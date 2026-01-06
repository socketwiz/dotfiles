# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Neovim configuration using `lazy.nvim` for plugin management.

## Structure

```
init.lua                 # Bootstrap lazy.nvim, load core options/autocmds/keymaps
lua/core.lua             # Editor defaults, provider settings (Python/Node paths)
lua/config/lazy.lua      # Plugin loading and performance config
lua/plugins/             # Plugin specs, one file per plugin (e.g., lsp-lua.lua)
lua/lsp/init.lua         # Shared LSP helpers (capabilities, on_attach)
snippets/                # LuaSnip snippet files (e.g., javascript.lua)
lazy-lock.json           # Pinned plugin versions
```

## Commands

```bash
nvim .              # Launch Neovim with this config
:Lazy sync          # Install/update plugins
:Lazy status        # Inspect plugin state and load timing
:checkhealth        # Run Neovim diagnostics
:TSUpdate           # Update Tree-sitter parsers
```

## Code Style

- 2-space indentation, tabs expanded (`expandtab`, `shiftwidth=2`)
- Lua modules live under `lua/` and are required by basename
- Plugin files: `lua/plugins/<feature>.lua`
- Format-on-save via `conform.nvim` (Prettier, Ruff, `cargo fmt`), LSP fallback

## Testing Changes

No automated tests. Validate by:
1. Opening Neovim
2. Checking `:messages` and `:checkhealth`
3. For formatter changes, save a sample buffer of the affected filetype

## Commit Style

Short, direct messages (often lowercase), no ticket prefixes.
