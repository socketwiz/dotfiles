
local opts = { noremap=true, silent=true }

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
end

local nvim_lsp = require('lspconfig')

-- Enable bash analyzer
-- npm i -g bash-language-server
nvim_lsp.bashls.setup {}

-- Enable html/css analyzer
-- npm i -g vscode-langservers-extracted
nvim_lsp.cssls.setup {}
nvim_lsp.html.setup {}
--
-- Enable javascript/typescript analyzer
nvim_lsp.denols.setup {
  on_attach = on_attach,
  init_options = {
    lint = true,
  },
}

-- Enable rust_analyzer
nvim_lsp.rust_analyzer.setup {}

