return {
  -- LazyVim owns nvim-lspconfig: capabilities (blink), on_attach keymaps,
  -- mason install, and setup. We only declare which servers to enable and
  -- their per-server settings via opts.servers (merged into LazyVim's list).
  "neovim/nvim-lspconfig",
  opts = {
    servers = {
      lua_ls = {
        settings = {
          Lua = {
            runtime = { version = "LuaJIT" },
            diagnostics = { globals = { "vim" } },
            workspace = { checkThirdParty = false },
            telemetry = { enable = false },
          },
        },
      },
      bashls = {},
      html = {},
      jsonls = {},
      ruff = {}, -- modern ruff LSP (replaces the deprecated ruff_lsp)
      ts_ls = {}, -- default root/cmd already match the old custom root_dir
      rust_analyzer = {
        settings = {
          ["rust-analyzer"] = {
            cargo = { allFeatures = true },
            checkOnSave = { command = "check" },
          },
        },
      },
    },
  },
}
