return {
  -- lualine is a LazyVim core plugin. Extend its opts (do NOT set `config`):
  -- override the theme with our midnight palette and show buffers in
  -- section A; LazyVim keeps ownership of the remaining sections.
  "nvim-lualine/lualine.nvim",
  opts = {
    options = {
      theme = {
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
      },
      component_separators = "|",
      section_separators = "",
    },
    sections = {
      lualine_a = { { "buffers" } },
    },
  },
}
