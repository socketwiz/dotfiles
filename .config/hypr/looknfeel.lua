-- Change the default Omarchy look'n'feel.

hl.config({
  general = {
    -- Required for the `immediate` window rule; tearing only applies to
    -- windows that opt in via hypr/windows.lua.
    allow_tearing = true,
  },

  misc = {
    -- Omarchy defaults to 1, which minimizes a fullscreen game when something
    -- in the background asks for focus. 2 keeps the game up.
    on_focus_under_fullscreen = 2,
  },

  cursor = {
    -- Force software cursors so the pointer survives monitor hotplug on NVIDIA.
    no_hardware_cursors = true,
  },
})
