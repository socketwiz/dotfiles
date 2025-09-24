return {
  "neovim/nvim-lspconfig",
  ft = "html",
  config = function()
    local lsp = require("lsp")

    vim.lsp.config("html", {
      capabilities = lsp.capabilities,
      on_attach = lsp.on_attach,
    })

    vim.lsp.enable("html")
  end,
}
