-- Keep only your personal input overrides here. Settings below replace
-- Omarchy's defaults in ~/.local/share/omarchy/default/hypr/input.lua.

hl.config({
  input = {
    -- Caps Lock acts as Ctrl. This replaces Omarchy's default of
    -- "compose:caps,shift:both_capslock_cancel", so Caps Lock is no longer
    -- the compose key and both-Shifts no longer toggles Caps Lock.
    kb_options = "ctrl:nocaps",

    -- Wait longer before a held key starts repeating (Omarchy default: 250).
    repeat_delay = 600,
  },
})

-- Corsair Nightsword DPI/speed is controlled by the hardware DPI buttons on the
-- mouse itself. Do not add a software sensitivity override here; input.sensitivity
-- stays at Omarchy's default of 0.

-- Omarchy already ships the terminal touchpad scroll rules this config used to
-- carry: (Alacritty|kitty|foot) at 1.5 and com.mitchellh.ghostty at 0.2.
