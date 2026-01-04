# Repository Guidelines

## Project Structure & Module Organization
- `init.lua` bootstraps `lazy.nvim`, then loads core options, autocmds, and keymaps.
- `lua/core.lua` stores editor defaults and provider settings.
- `lua/config/lazy.lua` configures plugin loading and performance.
- `lua/plugins/` holds plugin specs and language tooling (one file per plugin/LSP).
- `lua/lsp/` provides shared LSP helpers (`capabilities`, `on_attach`).
- `snippets/` contains LuaSnip snippet files (for example `snippets/javascript.lua`).
- `lazy-lock.json` pins plugin versions.

## Build, Test, and Development Commands
- `nvim .` launches Neovim with this configuration.
- `:Lazy sync` installs or updates plugins managed by `lazy.nvim`.
- `:Lazy status` inspects plugin state and load timing.
- `:checkhealth` runs Neovim diagnostics for providers and tooling.
- `:TSUpdate` updates Tree-sitter parsers.

## Coding Style & Naming Conventions
- Indentation is 2 spaces; tabs are expanded (`expandtab`, `shiftwidth=2`).
- Lua modules live under `lua/` and are required by basename.
- Plugin files follow `lua/plugins/<feature>.lua` naming (e.g., `lsp-lua.lua`).
- Format-on-save uses `conform.nvim` (Prettier, Ruff, `cargo fmt`), with LSP formatting as fallback.

## Testing Guidelines
- No automated test suite is defined here.
- Validate changes by opening Neovim and checking `:messages` and `:checkhealth`.
- For formatter changes, save a sample buffer of the affected filetype.

## Commit & Pull Request Guidelines
- Recent history shows short, direct commit messages (often lowercase) without ticket prefixes.
- If contributing via PR, include a concise summary and screenshots/GIFs for UI-visible changes.

## Configuration Notes
- LSP setup lives in `lua/plugins/lsp-*.lua`; shared hooks are in `lua/lsp/init.lua`.
- External tool paths (Python/Node) are set in `lua/core.lua` and may be system-specific.
