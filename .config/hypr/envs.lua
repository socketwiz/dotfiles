-- Personal environment overrides. Loaded after Omarchy's defaults, including
-- default/hypr/nvidia.lua, so values set here win.

-- These dotfiles run on more than one machine, so anything GPU specific has to
-- be decided at load time rather than hardcoded.
--
-- The laptop has Intel plus NVIDIA hybrid graphics. There, Omarchy's NVIDIA
-- detection sets LIBVA_DRIVER_NAME=nvidia, which makes Chrome and Google Meet
-- video flash and go black; pointing VA-API at the Intel iHD driver fixes it.
--
-- The desktop pairs the NVIDIA card with an AMD iGPU and has no Intel GPU and
-- no iHD driver, so forcing iHD there would break VA-API rather than fix it.
--
-- So only override when an Intel GPU is actually present and its VA-API driver
-- is installed. Otherwise Omarchy's nvidia default stands.
--
-- NVD_BACKEND and __GLX_VENDOR_LIBRARY_NAME come from Omarchy's NVIDIA
-- defaults and are left alone on both machines.

local INTEL_VENDOR_ID = "0x8086"
local INTEL_VAAPI_DRIVER = "/usr/lib/dri/iHD_drv_video.so"

local function file_exists(path)
  local handle = io.open(path, "r")
  if not handle then
    return false
  end
  handle:close()
  return true
end

local function read_trimmed(path)
  local handle = io.open(path, "r")
  if not handle then
    return nil
  end
  local line = handle:read("*l")
  handle:close()
  if not line then
    return nil
  end
  return (line:gsub("%s+", ""))
end

-- Walk the DRM cards rather than shelling out to lspci. Vendor ids live at
-- /sys/class/drm/cardN/device/vendor: 0x8086 Intel, 0x10de NVIDIA, 0x1002 AMD.
local function has_intel_gpu()
  for index = 0, 7 do
    if read_trimmed("/sys/class/drm/card" .. index .. "/device/vendor") == INTEL_VENDOR_ID then
      return true
    end
  end
  return false
end

if has_intel_gpu() and file_exists(INTEL_VAAPI_DRIVER) then
  hl.env("LIBVA_DRIVER_NAME", "iHD")
end
