return {
  "folke/noice.nvim",
  event = "VeryLazy",
  enabled = false,
  dependencies = {
    "MunifTanjim/nui.nvim",
    "rcarriga/nvim-notify",
  },
  config = function()
    require("noice").setup({
      messages = { enabled = false },
      lsp = {
        progress = { enabled = false },
        signature = { enabled = false },
        hover = { enabled = false },
        message = { enabled = false },
      },
      notify = {
        enabled = true,
      },
      throttle = 100,
      max_errors = 200,
      presets = {},
    })
  end,
}
