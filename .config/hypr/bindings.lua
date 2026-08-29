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

-- Discord push-to-talk: send F10 to Discord on both press and release instead
-- of letting Hyprland swallow it. The `pass` dispatcher would be the obvious
-- choice, but it is broken for binds registered from the Lua config: the
-- special-casing that makes it emit the release event is only applied to the
-- legacy text config (hyprwm/Hyprland discussion #14417), so push-to-talk
-- either never fires or sticks on. send_key_state takes the state explicitly,
-- so bind press to "down" and release to "up".
local ptt = function(state)
  return hl.dsp.send_key_state({
    mods = "",
    key = "F10",
    state = state,
    window = "class:discord",
  })
end

o.bind("F10", "Discord push-to-talk", ptt("down"))
o.bind("F10", nil, ptt("up"), { release = true })

-- Open the annotation editor on a screenshot straight away, as it did before
-- 4.0. Omarchy 4 leaves the editor behind a notification action instead, which
-- stays available on SUPER + ALT + comma for shots taken any other way.
hl.unbind("PRINT")
o.bind("PRINT", "Screenshot and edit", os.getenv("HOME") .. "/.local/bin/screenshot-edit")

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
