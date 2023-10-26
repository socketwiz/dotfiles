if require("utils").is_plugin_installed("lsp-zero.nvim") then
  local lsp_zero = require("lsp-zero").preset({
    name = "recommended",
    suggest_lsp_servers = false,
  })
  local lsp_capabilities = require("cmp_nvim_lsp").default_capabilities()
  local lspconfig = require("lspconfig")

  function on_attach(client, bufnr)
    local opts = { buffer = bufnr, remap = false }

    vim.keymap.set("n", "gd", function()
      vim.lsp.buf.definition()
    end, opts)
    vim.keymap.set("n", "K", function()
      vim.lsp.buf.hover()
    end, opts)
    vim.keymap.set("n", "<leader>ws", function()
      vim.lsp.buf.workspace_symbol()
    end, opts)
    vim.keymap.set("n", "<leader>d", function()
      vim.diagnostic.open_float()
    end, opts)
    vim.keymap.set("n", "[d", function()
      vim.diagnostic.goto_next()
    end, opts)
    vim.keymap.set("n", "]d", function()
      vim.diagnostic.goto_prev()
    end, opts)
    vim.keymap.set("n", "<leader>ca", function()
      vim.lsp.buf.code_action()
    end, opts)
    vim.keymap.set("n", "<leader>crf", function()
      vim.lsp.buf.references()
    end, opts)
    vim.keymap.set("n", "<leader>crn", function()
      vim.lsp.buf.rename()
    end, opts)
    vim.keymap.set("n", "<C-k>", function()
      vim.lsp.buf.signature_help()
    end, opts)
  end

  lsp_zero.ensure_installed({
    "eslint",
    "rust_analyzer",
    "tsserver",
  })

  lsp_zero.format_on_save({
    servers = {
      ["lua_ls"] = { "lua" },
      ["rust_analyzer"] = { "rust" },
    },
  })

  lsp_zero.configure("denols", {
    capabilities = lsp_capabilities,
    on_attach = on_attach,
    root_dir = lspconfig.util.root_pattern("deno.json", "deno.jsonc"),
  })

  lsp_zero.configure("tsserver", {
    capabilities = lsp_capabilities,
    filetypes = { "typescript", "typescriptreact", "typescript.tsx" },
    on_attach = on_attach,
    root_dir = lspconfig.util.root_pattern("package.json", "tsconfig.json", "jsconfig.json", ".git"),
  })

  lsp_zero.on_attach(on_attach)

  lsp_zero.setup()

  local rust_lsp = lsp_zero.build_options("rust_analyzer", {})

  require("rust-tools").setup({ server = rust_lsp })
end
