return {
  "neovim/nvim-lspconfig",
  ft = "sh",
  config = function()
    local lsp = require("lsp")

    vim.lsp.config("bashls", {
      capabilities = lsp.capabilities,
      on_attach = lsp.on_attach,
    })

    vim.lsp.enable("bashls")
  end,
}
