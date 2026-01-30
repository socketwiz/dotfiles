return {
  "folke/tokyonight.nvim",
  lazy = false,
  priority = 1000,
  config = function()
    require("tokyonight").setup({
      style = "night",
      on_colors = function(colors)
        colors.bg = "#000000"
        colors.bg_dark = "#000000"
        colors.bg_sidebar = "#000000"
        colors.bg_float = "#000000"
      end,
      on_highlights = function(hl, c)
        hl.DiagnosticHint = { fg = "orange" }
        hl.DiagnosticError = { fg = "#ff0000" }
        hl.CursorLine = { bg = "#0a0a2a" }
        hl["@markup.raw.markdown_inline"] = { bg = "#1a1a3a" }
      end,
      sidebars = { "qf", "vista_kind", "terminal", "packer" },
    })

    vim.cmd.colorscheme("tokyonight")
  end,
}

