-- Autocmds are defined in plugin configs (e.g., conform.nvim).

-- Native TypeScript 7 (tsgo) language server.
--
-- The global `typescript` v7 package ships a native Go LSP; `tsc --lsp --stdio`
-- forwards to it. There is no classic tsserver.js / typescript-language-server.
--
-- We attach it with an explicit FileType autocmd instead of LazyVim's
-- opts.servers path: LazyVim only auto-enables Mason-installed servers (via
-- mason-lspconfig), and `vim.lsp.enable` for a non-Mason server won't
-- retroactively attach to a buffer whose FileType already fired -- so the first
-- file opened on the command line would be missed. Registering here at startup
-- guarantees the autocmd is in place before any TS/JS buffer loads.
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "javascript", "javascriptreact", "typescript", "typescriptreact" },
  callback = function(ev)
    local config = {
      name = "tsgo",
      cmd = { "tsc", "--lsp", "--stdio" }, -- `tsc` on PATH via mise; survives node bumps
      root_dir = vim.fs.root(ev.buf, {
        "tsconfig.json", "jsconfig.json", "package.json", ".git",
      }),
    }
    -- Advertise blink.cmp's completion capabilities when available; degrade
    -- gracefully to nvim defaults if blink isn't loaded.
    local ok, blink = pcall(require, "blink.cmp")
    if ok then
      config.capabilities = blink.get_lsp_capabilities()
    end
    -- vim.lsp.start dedups by name/root_dir/cmd, so buffers in the same project
    -- reuse one client.
    vim.lsp.start(config)
  end,
})
