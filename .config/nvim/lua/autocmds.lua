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
    -- Only attach to real, file-backed buffers. Picker previews and other
    -- scratch buffers set a TS filetype on a `nofile` buffer with no path;
    -- letting one through sends a didOpen with a relative URI, which panics
    -- tsgo ("vfs: path is not absolute") and takes down every tsgo client in
    -- the session, not just this buffer. nvim's own vim.lsp.enable handler
    -- guards the same way.
    local bufname = vim.api.nvim_buf_get_name(ev.buf)
    if vim.bo[ev.buf].buftype ~= "" or bufname == "" then
      return
    end

    local config = {
      name = "tsgo",
      cmd = { "tsc", "--lsp", "--stdio" }, -- `tsc` on PATH via mise; survives node bumps
      -- Fall back to the buffer's own directory: vim.fs.root returns nil when
      -- none of the markers exist anywhere up the tree, and vim.lsp.start's
      -- reuse_client compares root_dir, so a nil root would make unrelated
      -- single-file buffers in different directories share one rootless client.
      root_dir = vim.fs.root(ev.buf, {
        "tsconfig.json", "jsconfig.json", "package.json", ".git",
      }) or vim.fs.dirname(bufname),
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

-- Code: Terraform game language server.
--
-- The game exports each save's scripts to a folder marked by
-- `codeterraform-workspace.json`, which records where the game installed the
-- server that matches its build. Read the path from there (as the game's own
-- Zed extension does) rather than hardcoding it, so game updates that move or
-- replace the server keep working. Python files outside a scripts folder are
-- left alone. Same FileType-autocmd approach as tsgo above, for the same reason.
vim.api.nvim_create_autocmd("FileType", {
  pattern = "python",
  callback = function(ev)
    local bufname = vim.api.nvim_buf_get_name(ev.buf)
    if vim.bo[ev.buf].buftype ~= "" or bufname == "" then
      return
    end

    local root = vim.fs.root(ev.buf, "codeterraform-workspace.json")
    if not root then
      return
    end

    local ok, workspace = pcall(function()
      local file = assert(io.open(root .. "/codeterraform-workspace.json"))
      local text = file:read("*a")
      file:close()
      return vim.json.decode(text)
    end)
    local server = ok and type(workspace.server) == "table" and workspace.server.path
    if type(server) ~= "string" or server == "" then
      vim.notify(
        "Code Terraform: no language server recorded in " .. root
          .. "; open this save in the game once",
        vim.log.levels.WARN
      )
      return
    end

    local config = {
      name = "codeterraform",
      cmd = { "node", server, "--stdio" }, -- needs Node 20+; `node` on PATH via mise
      root_dir = root,
    }
    local blink_ok, blink = pcall(require, "blink.cmp")
    if blink_ok then
      config.capabilities = blink.get_lsp_capabilities()
    end
    vim.lsp.start(config)
  end,
})
