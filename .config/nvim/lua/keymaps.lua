
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
-- Copy/paste to the clipboard
--map { 'n', 'y', '"+y' }
--map { 'n', 'p', '"+p' }
-- Format a json file with `jq`
map { 'n', '<leader>j', ':%!jq .<cr>' }

-- Telescope
map { 'n', '<leader>ff', '<cmd>lua require("telescope.builtin").find_files()<cr>' }
map { 'n', '<leader>fg', '<cmd>lua require("telescope.builtin").live_grep()<cr>' }
map { 'n', '<leader>fb', '<cmd>lua require("telescope.builtin").buffers()<cr>' }
map { 'n', '<leader>fh', '<cmd>lua require("telescope.builtin").help_tags()<cr>' }

-- LSP
map { 'n',  '<space>e', '<cmd>lua vim.diagnostic.open_float()<cr>' }
map { 'n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<cr>' }
map { 'n', ']d', '<cmd>lua vim.diagnostic.goto_next()<cr>' }
map { 'n', '<space>q', '<cmd>lua vim.diagnostic.setloclist()<cr>' }

-- Nvim tree
map { 'n', '<C-n>', '<cmd>NvimTreeToggle<cr>' }
map { 'n', '<leader>r', '<cmd>NvimTreeRefresh<cr>' }
map { 'n', '<C-x><C-j>', '<cmd>NvimTreeFindFile<cr>' }

-- Which key
local wk = require("which-key")
local wk_mappings = {
  q = { ':q<cr>', 'Quit' }
}
local wk_opts = {
  prefix = '<leader>'
}
wk.register(wk_mappings, wk_opts)

