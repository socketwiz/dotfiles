if require("utils").is_plugin_installed("lualine.nvim") then
  require('lualine').setup {
    options = {
      icons_enabled = true,
      theme = 'onedark',
      component_separators = '|',
      section_separators = '',
    },
    sections = {
      lualine_a = {
        {
          'buffers',
        }
      }
    }
  }
end
