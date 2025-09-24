return {
  "neovim/nvim-lspconfig",
  ft = "rust",
  config = function()
    local lsp = require("lsp")

    vim.lsp.config("rust_analyzer", {
      capabilities = lsp.capabilities,
      on_attach = function(client, bufnr)
        lsp.on_attach(client, bufnr)

        -- Disable LSP formatting; handled by conform.nvim
        client.server_capabilities.documentFormattingProvider = false
      end,
      root_dir = vim.fs.dirname(vim.fs.find({ "Cargo.toml", ".git" }, { upward = true })[1]),
      settings = {
        ["rust-analyzer"] = {
          cargo = { allFeatures = true },
          checkOnSave = { command = "clippy" },
        },
      },
    })

    vim.lsp.enable("rust_analyzer")
  end,
}

