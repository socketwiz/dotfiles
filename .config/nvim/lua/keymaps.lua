-- LazyVim's keymaps load after this file, so anything LazyVim also maps must be
-- re-asserted on VeryLazy (fires after LazyVim's keymaps land) to win. LazyVim
-- remaps <Up>/<Down> to gk/gj and <leader><space> to Find Files.
vim.api.nvim_create_autocmd("User", {
  pattern = "VeryLazy",
  callback = function()
    -- Disable arrow keys (train off them)
    vim.keymap.set("n", "<Up>", "<Nop>")
    vim.keymap.set("n", "<Down>", "<Nop>")
    vim.keymap.set("n", "<Left>", "<Nop>")
    vim.keymap.set("n", "<Right>", "<Nop>")
    -- Clear the search highlighting
    vim.keymap.set("n", "<leader><space>", ":noh<cr>", { silent = true, desc = "Clear search highlight" })
  end,
})

-- LSP keymaps
vim.keymap.set("n", "<leader>fd", function()
  vim.diagnostic.setqflist()
end, { desc = "LSP diagnostics to quickfix" })
