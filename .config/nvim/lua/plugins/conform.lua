return {
  "stevearc/conform.nvim",
  event = "BufWritePre",
  config = function()
    require("conform").setup({
      formatters = {
        cargo_fmt = {
          command = "cargo",
          args = { "fmt" },
          stdin = false,
        },
        ruff_imports = {
          command = "uv",
          args = { "run", "ruff", "check", "--select", "I", "--fix", "--stdin-filename", "$FILENAME", "-" },
          stdin = true,
        },
        ruff_format = {
          command = "uv",
          args = { "run", "ruff", "format", "--stdin-filename", "$FILENAME", "-" },
          stdin = true,
        },
      },
      formatters_by_ft = {
        javascript = { "prettier" },
        javascriptreact = { "prettier" },
        json = { "prettier" },
        python = { "ruff_imports", "ruff_format" },
        rust = { "cargo_fmt" },
      },
      format_on_save = {
        timeout_ms = 500,
        lsp_fallback = true,
      },
    })
  end,
}
