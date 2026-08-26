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

-- Discord push-to-talk: pass F10 through to Discord on both press and release
-- instead of letting Hyprland swallow it.
o.bind("F10", "Discord push-to-talk", hl.dsp.pass({ window = "class:discord" }))
o.bind("F10", nil, hl.dsp.pass({ window = "class:discord" }), { release = true })

-- Open the annotation editor on a screenshot straight away, as it did before
-- 4.0. Omarchy 4 leaves the editor behind a notification action instead, which
-- stays available on SUPER + ALT + comma for shots taken any other way.
hl.unbind("PRINT")
o.bind("PRINT", "Screenshot and edit", os.getenv("HOME") .. "/.local/bin/screenshot-edit")

-- Bindings dropped in the 4.0 port because Omarchy's defaults now match:
--   SUPER + SHIFT + F (nautilus --new-window), SHIFT + B / SHIFT + ALT + B
--   (browser), SHIFT + M (spotify), SHIFT + ALT + M (cliamp), SHIFT + N
--   (editor), SHIFT + D (docker TUI), SHIFT + G (signal), SHIFT + O (obsidian),
--   SHIFT + SLASH (1password), SHIFT + ALT + A (grok), SHIFT + C / SHIFT + E
--   (hey), SHIFT + Y (youtube), SHIFT + ALT + G (whatsapp), SHIFT + CTRL + G
--   (google messages), SHIFT + P (google photos), SHIFT + X / SHIFT + ALT + X.
--
-- Also dropped:
--   * SUPER + SHIFT + R -> ~/.local/bin/obs-record-toggle. That script does not
--     exist on this machine.
