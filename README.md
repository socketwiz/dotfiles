# 🌱 Dotfiles — Ricky Nelson

Configuration files for my development environment using a Git bare repo strategy.

---

## 🧰 Installation

```sh
git clone --bare git@github.com:socketwiz/dotfiles.git $HOME/.dotfiles
```

### Add this function to your shell config (`.zshrc` or `.bashrc`):

```sh
function git() {
  # Check if you're inside a regular git repo
  if git rev-parse --is-inside-work-tree > /dev/null 2>&1; then
    command git "$@"
  else
    # Use the dotfiles repository when outside another git repo
    /usr/bin/git --git-dir="$HOME/.dotfiles" --work-tree="$HOME" "$@"
  fi
}
```

Then run:

```sh
git checkout
```

---


## 🔐 Secret Management

Secrets like API keys are stored in files excluded from Git:

```sh
~/.zshenv.local
```

Sourced via:

```sh
[[ -f ~/.zshenv.local ]] && source ~/.zshenv.local
```

---

## 🧠 Philosophy

Keep only what you need.
Track only what you trust.
Automate the rest.

—
Ricky Nelson ✈️
