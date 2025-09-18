return {
  "folke/tokyonight.nvim",
  lazy = false,
  priority = 1000,
  config = function()
    vim.g.tokyonight_colors = {
      hint = "orange",
      error = "#ff0000",
    }
    vim.g.tokyonight_style = "night"
    vim.g.tokyonight_sidebars = { "qf", "vista_kind", "terminal", "packer" }

    vim.cmd.colorscheme("tokyonight")
  end,
}

