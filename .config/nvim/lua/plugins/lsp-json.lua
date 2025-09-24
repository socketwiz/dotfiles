return {
  "neovim/nvim-lspconfig",
  ft = "json",
  config = function()
    local lsp = require("lsp")

    vim.lsp.config("jsonls", {
      capabilities = lsp.capabilities,
      on_attach = lsp.on_attach,
    })

    vim.lsp.enable("jsonls")
  end,
}
