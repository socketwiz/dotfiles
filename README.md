# Dotfiles — Ricky Nelson

Configuration files for my development environment using a Git bare repo strategy.

---

## Installation

```sh
# Clone the bare repo
git clone --bare git@github.com:socketwiz/dotfiles.git $HOME/.dotfiles

# Define a temporary alias
alias dotfiles='/usr/bin/git --git-dir="$HOME/.dotfiles" --work-tree="$HOME"'

# Backup any conflicting files
dotfiles checkout 2>&1 | grep -E "^\s+" | awk '{print $1}' | \
  xargs -I{} mv {} {}.bak

# Checkout the dotfiles
dotfiles checkout

# Don't show untracked files (reduces noise in home directory)
dotfiles config status.showUntrackedFiles no

# Restart your shell
exec zsh
```

After restart, the `git` function in `.zshrc` automatically uses the dotfiles repo when in `$HOME`, `$HOME/.config`, or `$HOME/.emacs.d`.

---

## Tools & Features

### Prompt & Shell
- **Starship** — cross-shell prompt
- **zoxide** — smart directory jumping (use `z` to jump)
- **fzf** — fuzzy finder integration

### CLI Replacements
| Original | Replacement |
|----------|-------------|
| `cat`    | `bat`       |
| `ls`     | `eza --git` |
| `vim`    | `nvim`      |

### Version Management
- **mise** — runtime version manager
- **pyenv** — Python version management (lazy loaded)

### Aliases

**Git:**
`ga` (add), `gc` (commit), `gco` (checkout), `gcb` (checkout -b), `gd` (diff), `gl` (pull), `gp` (push), `gst` (status), `glog` (log graph)

**Docker:**
`sdc` (sudo docker compose), `dcup`, `dcstop`, `dclogs`, `dcps`, `dcdestroy`

**Safety:**
`rm -I`, `mv -i`, `cp -i`

### Emacs
- `ed` — start daemon (`emacs --daemon`)
- `ec` — connect to daemon (`emacsclient -c -nw`)
- `et` — terminal mode (`emacs --no-window-system`)

---

## Tracked Configurations

### Shell
`.zshrc`, `.zshenv`, `.zprofile`, `.bashrc`, `.bash_profile`, `.bash_logout`, `.profile`

### Platform-specific
`.darwin`, `.linux`, `.sun` — sourced based on `uname`

### Editors
- `.emacs.d/*.el` — Emacs config
- `.config/nvim/` — Neovim config
- `.config/Code/` — VS Code settings
- `.config/helix/` — Helix editor

### Terminals
`.config/alacritty/`, `.config/kitty/`, `.config/wezterm/`, `.config/termite/`, `.config/iterm2/`

### Window Managers & Desktop
`.config/i3/`, `.config/i3status-rs/`, `.config/sway/`, `.config/waybar/`, `.config/rofi/`, `.config/dunst/`, `.config/picom.conf`

### Other
- `.config/tmux/`, `.config/zellij/` — terminal multiplexers
- `.config/fish/`, `.config/nushell/` — alternative shells
- `.config/gitui/` — git TUI
- `.config/starship.toml` — prompt config
- `.ssh/config` — SSH configuration
- `.local/bin/` — custom scripts
- `.deno/` — Deno runtime

---

## Secret Management

Secrets like API keys are stored in files excluded from Git:

```sh
~/.zshrc.local
~/.zshenv.local
```

Sourced at the end of `.zshrc`:

```sh
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local
```

---

## Philosophy

Keep only what you need.
Track only what you trust.
Automate the rest.

—
Ricky Nelson
