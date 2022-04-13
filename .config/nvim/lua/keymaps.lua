
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
local wk = require("which-key")
local wk_mappings = {
  c = { ':e ~/.config/nvim/init.lua<cr>', 'Edit config' },
  f = {
    name = 'Telescope',
    b = { '<cmd>lua require("telescope.builtin").buffers()<cr>', 'Buffer list' },
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

