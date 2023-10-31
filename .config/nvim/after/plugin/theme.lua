if require("utils").is_plugin_installed("tokyonight.nvim") then
  local colorscheme_name = 'tokyonight'

  -- Change the "hint" color to the "orange" color, and make the "error" color bright red
  vim.g.tokyonight_colors = { hint = "orange", error = "#ff0000" }
  vim.g.tokyonight_style = "night"
  vim.g.tokyonight_sidebars = { "qf", "vista_kind", "terminal", "packer" }
  --vim.o.termguicolors = true
  vim.cmd('colorscheme ' .. colorscheme_name)
end
