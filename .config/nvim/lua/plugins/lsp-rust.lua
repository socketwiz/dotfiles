return {
  "neovim/nvim-lspconfig",
  ft = "rust",
  config = function()
    local lsp = require("lsp")

    vim.lsp.config("rust_analyzer", {
      capabilities = lsp.capabilities,
      on_attach = lsp.on_attach,
      settings = {
        ["rust-analyzer"] = {
          cargo = {
            allFeatures = true,
            buildScripts = { enable = false },
          },
          checkOnSave = {
            command = "clippy",
          },
          procMacro = { enable = false },
        },
      },
    })

    vim.lsp.enable("rust_analyzer")
  end,
}
