-- Personal window rules. Loaded after Omarchy's defaults in
-- ~/.local/share/omarchy/default/hypr/windows.lua, so these win.
--
-- See https://wiki.hypr.land/Configuring/Basics/Window-Rules/

-- btop needs more room than Omarchy's standard 875x600 float, so drop it out of
-- the floating-window tag and size it directly.
o.window("org.omarchy.btop", { tag = "-floating-window" })
o.window("org.omarchy.btop", { float = true })
o.window("org.omarchy.btop", { center = true })
o.window("org.omarchy.btop", { size = { 1800, 1200 } })

-- Enshrouded: allow tearing, keep rendering while unfocused, never idle out.
o.window("steam_app_2079670", { immediate = true })
o.window("steam_app_2079670", { render_unfocused = true })
o.window("steam_app_2079670", { idle_inhibit = "always" })

-- WoW Classic under Lutris/Wine: fullscreen windowed with no Waybar gap.
o.window({ class = "steam_app_default", title = "World of Warcraft" }, { fullscreen = true })
o.window({ class = "steam_app_default", title = "World of Warcraft" }, { immediate = true })
o.window({ class = "steam_app_default", title = "World of Warcraft" }, { idle_inhibit = "always" })

-- Rules dropped in the 4.0 port because Omarchy now covers them:
--   * org.omarchy.screensaver fullscreen/float -- default/hypr/apps/system.lua
--   * steam_app opacity 1 1 -- default/hypr/apps/steam.lua matches "steam.*"
--   * steam_app no_blur / no_shadow -- blur and shadow are off globally
--   * steam_app focus_on_activate -- misc.focus_on_activate is true globally
