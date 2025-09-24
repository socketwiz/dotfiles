local M = {}

M.capabilities = require("cmp_nvim_lsp").default_capabilities()

M.on_attach = function(client, bufnr)
  local opts = { buffer = bufnr, silent = true, noremap = true }
  local map = vim.keymap.set

  map("n", "gd", vim.lsp.buf.definition, opts)
  map("n", "K", vim.lsp.buf.hover, opts)
  map("n", "[d", vim.diagnostic.goto_prev, opts)
  map("n", "]d", vim.diagnostic.goto_next, opts)
  map("n", "<leader>ca", vim.lsp.buf.code_action, opts)
  map("n", "<leader>crn", vim.lsp.buf.rename, opts)
  map("n", "<leader>d", vim.diagnostic.open_float, opts)
  map("n", "<leader>ws", vim.lsp.buf.workspace_symbol, opts)
  map("n", "<C-k>", vim.lsp.buf.signature_help, opts)
end

return M

