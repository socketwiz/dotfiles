
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

local telescope = require('telescope.builtin')
vim.keymap.set('n', '<leader>f', telescope.find_files, {})
vim.keymap.set('n', '<C-p>', telescope.git_files, {})
vim.keymap.set('n', '<leader>s', function()
	telescope.grep_string({ search = vim.fn.input("Grep > ") })
end)
vim.keymap.set('n', '<leader>h', telescope.help_tags, {})

