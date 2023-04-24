if require("utils").is_plugin_installed("lsp-zero.nvim") then
  local lsp = require("lsp-zero").preset({
    name = "recommended",
    suggest_lsp_servers = false,
  })

  lsp.ensure_installed({
    "eslint",
    "rust_analyzer",
    "tsserver",
  })

  lsp.format_on_save({
    servers = {
      ["lua_ls"] = { "lua" },
      ["rust_analyzer"] = { "rust" },
    },
  })

  lsp.on_attach(function(client, bufnr)
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
  end)

  lsp.setup()

  local rust_lsp = lsp.build_options("rust_analyzer", {})

  require("rust-tools").setup({ server = rust_lsp })
end
