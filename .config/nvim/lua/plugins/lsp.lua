return {
  "neovim/nvim-lspconfig",
  dependencies = {
    "hrsh7th/cmp-nvim-lsp",
    { "VonHeikemen/lsp-zero.nvim", branch = "v3.x" },
  },
  config = function()
    local lsp_zero = require("lsp-zero").preset({
      name = "minimal",
      set_lsp_keymaps = true,
      manage_nvim_cmp = false,
      suggest_lsp_servers = false,
    })

    local capabilities = require("cmp_nvim_lsp").default_capabilities()

    local function on_attach(_, bufnr)
      local opts = { buffer = bufnr }
      vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
      vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
      vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, opts)
      vim.keymap.set("n", "]d", vim.diagnostic.goto_next, opts)
      vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, opts)
      vim.keymap.set("n", "<leader>crn", vim.lsp.buf.rename, opts)
      vim.keymap.set("n", "<leader>d", vim.diagnostic.open_float, opts)
      vim.keymap.set("n", "<leader>ws", vim.lsp.buf.workspace_symbol, opts)
      vim.keymap.set("n", "<C-k>", vim.lsp.buf.signature_help, opts)
    end

    lsp_zero.format_on_save({
      servers = {
        lua_ls = { "lua" },
        pylsp = { "python" },
        rust_analyzer = { "rust" },
      },
    })

    local servers = {
      lua_ls = {
        settings = {
          Lua = {
            diagnostics = { enable = false },
          },
        },
      },
      ts_ls = {},
      html = {},
      jsonls = {},
      bashls = {},
      pylsp = {
        settings = {
          pylsp = {
            plugins = {
              pycodestyle = { enabled = false },
              pylint = { enabled = true },
              yapf = { enabled = false },
            },
          },
        },
      },
    }

    for name, opts in pairs(servers) do
      opts.capabilities = capabilities
      opts.on_attach = on_attach
      vim.lsp.config(name, opts)
      vim.lsp.enable(name)
    end

    -- Rust, without rust-tools
    vim.lsp.config("rust_analyzer", {
      settings = {
        ["rust-analyzer"] = {
          cargo = { allFeatures = true },
          checkOnSave = { command = "clippy" },
        },
      },
      capabilities = capabilities,
      on_attach = on_attach,
    })
    vim.lsp.enable("rust_analyzer")
  end,
}
