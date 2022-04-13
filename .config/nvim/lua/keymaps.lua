
-- Shamlessly stole this `map` function from
-- https://vonheikemen.github.io/devlog/tools/configuring-neovim-using-lua/
local map = function(key)
  -- get the extra options
  local opts = {noremap = true}
  for i, v in pairs(key) do
    if type(i) == 'string' then opts[i] = v end
  end

  -- basic support for buffer-scoped keybindings
  local buffer = opts.buffer
  opts.buffer = nil

  if buffer then
    vim.api.nvim_buf_set_keymap(0, key[1], key[2], key[3], opts)
  else
    vim.api.nvim_set_keymap(key[1], key[2], key[3], opts)
  end
end

map { 'n', '<Up>', '<Nop>' }
map { 'n', '<Down>', '<Nop>' }
map { 'n', '<Left>', '<Nop>' }
map { 'n', '<Right>', '<Nop>' }
-- Clear the search highlighting
map { 'n', '<leader><space>', ':noh<cr>' }

-- Nvim tree
map { 'n', '<C-x><C-j>', '<cmd>NvimTreeFindFile<cr>' }

-- Which key
local wk = require('which-key')
local wk_mappings = {
  c = { ':e ~/.config/nvim/init.lua<cr>', 'Edit config' },
  d = {
    name = 'Debugger',
    b = { '<cmd>lua require("dap").toggle_breakpoint()<cr>', 'Set breakpoint' },
    c = { '<cmd>lua require("dap").continue()<cr>', 'Debug continue' },
    i = { '<cmd>lua require("dap").step_into()<cr>', 'Debug step into' },
    l = { '<cmd>lua require("dap").launch()<cr>', 'Launch debug session' },
    n = { '<cmd>lua require("dap").run()<cr>', 'Run' },
    o = { '<cmd>lua require("dap").step_over()<cr>', 'Debug step over' },
    r = { '<cmd>lua require("dap").repl.open()<cr>', 'REPL' },
  },
  f = {
    name = 'Telescope',
    b = { '<cmd>lua require("telescope.builtin").buffers()<cr>', 'Buffer list' },
    c = { '<cmd>lua require("telescope.builtin").commands()<cr>', 'Commands' },
    f = { '<cmd>lua require("telescope.builtin").find_files()<cr>', 'Find file' },
    g = { '<cmd>lua require("telescope.builtin").live_grep()<cr>', 'Ripgrep' },
    h = { '<cmd>lua require("telescope.builtin").help_tags()<cr>', 'Help' }
  },
  j = { ':%!jq .<cr>', 'Format a JSON file' },
  l = {
    name = 'LSP',
    l = { '<cmd>lua vim.diagnostic.setloclist()<cr>', 'Open diagnostic location list' },
    n = { '<cmd>lua vim.diagnostic.goto_prev()<cr>', 'Next diagnostic' },
    p = { '<cmd>lua vim.diagnostic.goto_next()<cr>', 'Previous diagnostic' },
    w = { '<cmd>lua vim.diagnostic.open_float()<cr>', 'Open floating diagnostic window' }
  },
  t = {
    name = 'Nvim tree',
    r = { '<cmd>NvimTreeRefresh<cr>', 'Refresh tree' },
    t = { '<cmd>NvimTreeToggle<cr>', 'Toggle tree' }
  },
  u = {
    name = 'Utilities',
    r = { ':set invrelativenumber<cr>', 'Toggle relative line numbers' }
  },
  q = { ':q<cr>', 'Quit' }
}
local wk_opts = {
  prefix = '<leader>'
}
wk.register(wk_mappings, wk_opts)

local cmp = require('cmp')
cmp.setup({
  mapping = {
    ['<C-b>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
    ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
    ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),
    ['<C-y>'] = cmp.config.disable, -- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
    ['<C-e>'] = cmp.mapping({
      i = cmp.mapping.abort(),
      c = cmp.mapping.close(),
    }),
    ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
  }
})

