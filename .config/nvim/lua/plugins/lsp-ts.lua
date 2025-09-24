return {
  "neovim/nvim-lspconfig",
  ft = { "javascript", "typescript" },
  config = function()
    local lsp = require("lsp")

    vim.lsp.config("tsserver", {
      capabilities = lsp.capabilities,
      on_attach = lsp.on_attach,
      root_dir = vim.fs.dirname(vim.fs.find({ "package.json", "tsconfig.json", "jsconfig.json", ".git" },
        { upward = true })[1]),
    })

    vim.lsp.enable("tsserver")
  end,
}
