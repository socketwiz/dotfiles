return {
  "neovim/nvim-lspconfig",
  ft = "lua",
  config = function()
    local lsp = require("lsp")

    vim.lsp.config("lua_ls", {
      capabilities = lsp.capabilities,
      on_attach = lsp.on_attach,
      settings = {
        Lua = {
          runtime = {
            version = "LuaJIT",
          },
          diagnostics = {
            globals = { "vim" },
          },
          workspace = {
            checkThirdParty = false,
          },
          telemetry = {
            enable = false,
          },
        },
      },
    })

    vim.lsp.enable("lua_ls")
  end,
}
