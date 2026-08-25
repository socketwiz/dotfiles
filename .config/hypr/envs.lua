-- Personal environment overrides. Loaded after Omarchy's defaults, including
-- default/hypr/nvidia.lua, so values set here win.

-- This machine has an NVIDIA GPU with GSP firmware, so Omarchy's NVIDIA
-- detection sets LIBVA_DRIVER_NAME=nvidia. On this hybrid-graphics setup that
-- makes Chrome and Google Meet video flash and go black. Point VA-API at the
-- Intel iHD driver instead.
--
-- NVD_BACKEND=direct and __GLX_VENDOR_LIBRARY_NAME=nvidia are already set by
-- Omarchy's NVIDIA defaults and are left alone.
hl.env("LIBVA_DRIVER_NAME", "iHD")
