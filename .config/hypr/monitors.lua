-- See https://wiki.hypr.land/Configuring/Basics/Monitors/
-- List current monitors and supported resolutions with: hyprctl monitors all

-- These dotfiles run on both the laptop and the desktop, which want different
-- scaling, so pick at load time instead of hardcoding one machine's choice.
--
-- An eDP connector is an internal laptop panel. Only the laptop has one, so it
-- is a reliable way to tell the two apart without hardcoding a hostname.
local function has_internal_panel()
  local pipe = io.popen("ls -1 /sys/class/drm 2>/dev/null")
  if not pipe then
    return false
  end
  local found = false
  for name in pipe:lines() do
    if name:lower():match("edp") then
      found = true
      break
    end
  end
  pipe:close()
  return found
end

-- Steam is X11 only, so it always runs under XWayland and never sees the
-- Wayland fractional scale. Left alone it renders at 1x and comes out too small
-- on a scaled display, so tell it the scale explicitly.
if has_internal_panel() then
  -- Laptop: retina-class internal panel, 2x scaling throughout.
  local omarchy_gdk_scale = 2
  local omarchy_monitor_scale = "auto"

  hl.env("GDK_SCALE", tostring(omarchy_gdk_scale))
  hl.env("STEAM_FORCE_DESKTOPUI_SCALING", "2")
  hl.monitor({ output = "", mode = "preferred", position = "auto", scale = omarchy_monitor_scale })
else
  -- Desktop: 28" 4K BenQ RD280U. A 1.25 fractional scale relies on Wayland
  -- fractional-scale-v1 for GTK, so GDK_SCALE stays unset. Leaving it at 2
  -- while the monitor scales 1.6 makes GTK apps oversized and blurry.
  hl.env("STEAM_FORCE_DESKTOPUI_SCALING", "1.25")
  hl.monitor({ output = "", mode = "preferred", position = "auto", scale = 1.25 })
end

-- Configure a specific monitor.
-- hl.monitor({ output = "DP-2", mode = "2560x1440@144", position = "0x0", scale = 1 })

-- Portrait/rotated secondary monitor (transform: 1 = 90, 3 = 270).
-- hl.monitor({ output = "DP-2", mode = "preferred", position = "auto", scale = 1, transform = 1 })
