local colorscheme_name = 'tokyonight'

local function load_colorscheme()
  local found = false
  local paths = vim.api.nvim_get_runtime_file('pack/packer/start/tokyonight.nvim', false)

  if #paths > 0 then
    found = true
  end

  if found then
    -- Change the "hint" color to the "orange" color, and make the "error" color bright red
    vim.g.tokyonight_colors = { hint = "orange", error = "#ff0000" }
    vim.g.tokyonight_style = "night"
    vim.g.tokyonight_sidebars = { "qf", "vista_kind", "terminal", "packer" }
    --vim.o.termguicolors = true
    vim.cmd('colorscheme ' .. colorscheme_name)
  else
    print('Colorscheme "' .. colorscheme_name .. '" not found. Falling back to default.')
  end
end

load_colorscheme()
