local lsp = require('lsp-zero').preset({
  name = 'minimal',
  set_lsp_keymaps = true,
  manage_nvim_cmp = true,
  suggest_lsp_servers = false,
})

lsp.ensure_installed({
  'eslint',
  'rust_analyzer',
  'tsserver',
})

lsp.setup_servers({'eslint', 'rust_analyzer', 'tsserver' })

lsp.setup()
