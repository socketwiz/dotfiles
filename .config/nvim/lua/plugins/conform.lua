return {
  -- LazyVim already loads and configures conform.nvim (setup, format-on-save,
  -- <leader>cf, LSP fallback). We only extend it with our custom formatters
  -- and filetype mappings via opts -- do NOT set `config`, it breaks LazyVim.
  "stevearc/conform.nvim",
  opts = {
    notify_on_error = true, -- show popup if a formatter fails
    formatters = {
      cargo_fmt = {
        command = "cargo",
        args = { "fmt", "--", "$FILENAME" },
        stdin = false,
        tempfile_postfix = ".rs", -- Use temp file to avoid in-place write conflict
      },
      ruff_imports = {
        command = "uv",
        args = {
          "run", "ruff", "check", "--select", "I", "--fix",
          "--stdin-filename", "$FILENAME", "-",
        },
        stdin = true,
      },
      ruff_format = {
        command = "uv",
        args = { "run", "ruff", "format", "--stdin-filename", "$FILENAME", "-" },
        stdin = true,
      },
    },
    -- which formatters to run by filetype (merged into LazyVim's builtin list)
    formatters_by_ft = {
      javascript = { "prettier" },
      javascriptreact = { "prettier" },
      json = { "prettier" },
      python = { "ruff_format", "ruff_imports" }, -- run ruff_format first, then ruff_imports
      rust = { "cargo_fmt" },
    },
  },
}
