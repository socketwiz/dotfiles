return {
  "neovim/nvim-lspconfig",
  ft = "python",
  config = function()
    local lsp = require("lsp")

    vim.lsp.config("pylsp", {
      capabilities = lsp.capabilities,
      on_attach = lsp.on_attach,
      settings = {
        pylsp = {
          plugins = {
            pycodestyle = { enabled = false },
            pylint = { enabled = true },
            yapf = { enabled = false },
          },
        },
      },
    })

    vim.lsp.enable("pylsp")
  end,
}
