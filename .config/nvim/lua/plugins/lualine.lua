return {
  "nvim-lualine/lualine.nvim",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  event = "VeryLazy",
  config = function()
    local midnight = {
      normal = {
        a = { fg = "#000000", bg = "#90b8ff", gui = "bold" },
        b = { fg = "#c8d3f5", bg = "#3b4261" },
        c = { fg = "#b4bcd8", bg = "#000000" },
      },
      insert = {
        a = { fg = "#000000", bg = "#5ee8cd", gui = "bold" },
      },
      visual = {
        a = { fg = "#000000", bg = "#cdb0ff", gui = "bold" },
      },
      replace = {
        a = { fg = "#000000", bg = "#f7768e", gui = "bold" },
      },
      command = {
        a = { fg = "#000000", bg = "#a0e8ff", gui = "bold" },
      },
      inactive = {
        a = { fg = "#636da5", bg = "#000000" },
        b = { fg = "#636da5", bg = "#000000" },
        c = { fg = "#636da5", bg = "#000000" },
      },
    }

    require("lualine").setup({
      options = {
        icons_enabled = true,
        theme = midnight,
        component_separators = "|",
        section_separators = "",
      },
      sections = {
        lualine_a = {
          { "buffers" },
        },
      },
    })
  end,
}
