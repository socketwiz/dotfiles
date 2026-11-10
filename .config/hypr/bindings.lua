-- Keep only your personal keybinding overrides here. Add new bindings or
-- unbind defaults before replacing them.
--
-- See current bindings and descriptions:
--   omarchy menu keybindings --print

-- Typora instead of Omarchy's Omawrite.
hl.unbind("SUPER + SHIFT + W")
o.bind("SUPER + SHIFT + W", "Typora", { launch = "typora --enable-wayland-ime" })

-- Claude instead of Omarchy's ChatGPT.
hl.unbind("SUPER + SHIFT + A")
o.bind("SUPER + SHIFT + A", "Claude", { webapp = "https://claude.ai" })

-- Activity (btop) on SUPER + SHIFT + T, as before 4.0. Omarchy 4 moved this to
-- SUPER + CTRL + T, which stays bound as well.
o.bind("SUPER + SHIFT + T", "Activity", { tui = "btop" })

-- Discord push-to-talk. Discord 1.0.166 registers push-to-talk through the
-- GlobalShortcuts portal and ignores raw key events sent to its window, so
-- trigger the portal shortcut with the `global` dispatcher. Discord shows
-- "Your desktop hasn't allowed this shortcut yet" until a key is bound here.
o.bind("F10", "Discord push-to-talk", hl.dsp.global("discord:discord-push-to-talk"))

-- Open the annotation editor on a screenshot straight away, as it did before
-- 4.0. Omarchy's default PRINT only copies and shows a preview with an Edit
-- button. In the editor, Enter copies and saves.
hl.unbind("PRINT")
o.bind("PRINT", "Screenshot and edit", "omasnap --editor overlay")

-- Thunar instead of Omarchy's Files (nautilus), for both the plain and the
-- cwd-aware file manager bindings.
hl.unbind("SUPER + SHIFT + F")
o.bind("SUPER + SHIFT + F", "File manager", { launch = "thunar" })

hl.unbind("SUPER + ALT + SHIFT + F")
o.bind("SUPER + ALT + SHIFT + F", "File manager (cwd)", 'uwsm-app -- thunar "$(omarchy-cmd-terminal-cwd)"')

-- Toggle OBS recording, as before 4.0. The 4.0 port dropped this on the
-- assumption the script was missing, but it exists on this machine.
o.bind("SUPER + SHIFT + R", "Toggle recording", os.getenv("HOME") .. "/.local/bin/obs-record-toggle")

-- Bindings dropped in the 4.0 port because Omarchy's defaults now match:
--   SHIFT + B / SHIFT + ALT + B
--   (browser), SHIFT + M (spotify), SHIFT + ALT + M (cliamp), SHIFT + N
--   (editor), SHIFT + D (docker TUI), SHIFT + G (signal), SHIFT + O (obsidian),
--   SHIFT + SLASH (1password), SHIFT + ALT + A (grok), SHIFT + C / SHIFT + E
--   (hey), SHIFT + Y (youtube), SHIFT + ALT + G (whatsapp), SHIFT + CTRL + G
--   (google messages), SHIFT + P (google photos), SHIFT + X / SHIFT + ALT + X.
