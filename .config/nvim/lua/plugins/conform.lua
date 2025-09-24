return {
  "stevearc/conform.nvim",
  event = { "BufReadPre", "BufNewFile" },
  config = function()
    require("conform").setup({
      notify_on_error = true,           -- show popup if a formatter fails
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
            "--stdin-filename", "$FILENAME", "-"
          },
          stdin = true,
        },
        ruff_format = {
          command = "uv",
          args = { "run", "ruff", "format", "--stdin-filename", "$FILENAME", "-" },
          stdin = true,
        },
      },
      -- which formatters to run by filetype
      formatters_by_ft = {
        javascript = { "prettier" },
        javascriptreact = { "prettier" },
        json = { "prettier" },
        python = { "ruff_format", "ruff_imports" }, -- run ruff_format first, then ruff_imports
        rust = { "cargo_fmt" },
      },
    })

    -- custom format-on-save (instead of conform.format_on_save)
    -- gives cleaner error messages and full control
    vim.api.nvim_create_autocmd("BufWritePre", {
      callback = function(args)
        local ok, err = pcall(function()
          require("conform").format({
            bufnr = args.buf,
            timeout_ms = 500,
            lsp_fallback = true,   -- fallback to LSP formatting if no formatter is found
            stop_after_first = true, -- stop after the first successful formatter
          })
        end)
        if not ok then
          vim.notify("Conform error: " .. err, vim.log.levels.ERROR)
        end
      end,
    })
  end,
}

