return {
  "neovim/nvim-lspconfig",
  ft = "python",
  config = function()
    local lsp = require("lsp")

    vim.lsp.config("ruff_lsp", {
      capabilities = lsp.capabilities,
      on_attach = lsp.on_attach,
      cmd = { "ruff-lsp" },
    })

    vim.lsp.enable("ruff_lsp")
  end,
}
