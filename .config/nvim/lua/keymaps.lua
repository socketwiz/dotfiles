vim.keymap.set("n", "<Up>", "<Nop>")
vim.keymap.set("n", "<Down>", "<Nop>")
vim.keymap.set("n", "<Left>", "<Nop>")
vim.keymap.set("n", "<Right>", "<Nop>")

-- Clear the search highlighting
vim.keymap.set("n", "<leader><space>", ":noh<cr>")

-- LSP keymaps
vim.keymap.set("n", "<leader>fd", function()
  vim.diagnostic.setqflist()
end, { desc = "LSP diagnostics to quickfix" })
