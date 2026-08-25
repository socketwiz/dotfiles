-- Extra autostart processes.

local home = os.getenv("HOME")

-- Disable the trackpad while an external mouse is plugged in.
o.launch_on_start(home .. "/.local/bin/touchpad-auto-toggle")

-- udiskie is no longer started here; Omarchy's own autostart runs it as
-- "udiskie --automount --no-notify --no-tray".

-- ckb-next (Corsair keyboard/mouse daemon) is not installed on this machine
-- any more. Uncomment if you reinstall it.
-- if o.cmd_present("ckb-next") then
--   o.launch_on_start("ckb-next --background")
-- end
