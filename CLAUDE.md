# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Structure

This is a dotfiles repository using a **bare Git repo strategy**. The bare Git repo is stored in `~/.dotfiles` with the home directory as the working tree.

## Commands

### Git operations in this repo
The `.zshrc` defines a custom `git` function that automatically uses the dotfiles repo when in `$HOME`, `$HOME/.config`, or `$HOME/.emacs.d`:
```sh
# Regular git command - works in these directories for dotfiles
git status
git add <file>
git commit -m "message"
```

### Manually use dotfiles repo
```sh
/usr/bin/git --git-dir="$HOME/.dotfiles" --work-tree="$HOME" <command>
```

## Architecture

### File organization
- **Shell config chain**: `.zshenv` (env vars) → `.zshrc` (interactive) → platform-specific (`.linux` or `.darwin`)
- **Local overrides**: `.zshrc.local` and `.zshenv.local` for machine-specific configs (not tracked)
- **Tool version management**: Uses `mise` and `pyenv`

### Tracked configurations
The `.gitignore` uses an allowlist approach - everything is ignored except explicitly allowed paths:
- Shell: `.zshrc`, `.zshenv`, `.zprofile`, `.bashrc`, `.bash_profile`, `.bash_logout`
- Editors: `.emacs.d/*.el`, `.config/nvim/`, `.config/Code/`, `.config/helix/`
- Terminals: `.config/ghostty/`, `.config/iterm2/`
- Window managers: `.config/hypr/`, `.config/waybar/`, `.config/swayosd/`, `.config/omarchy/`
- Other: `.config/tmux/`, `.config/gitui/`, `.config/starship.toml`

### CLI tool replacements
- `cat` → `bat`
- `ls` → `eza --git`
- `vim` → `nvim`

### Editor setup
Default editor is `nvim`. Emacs is used via daemon mode (`emacs --daemon`) with `emacsclient -c -nw` for sessions.
