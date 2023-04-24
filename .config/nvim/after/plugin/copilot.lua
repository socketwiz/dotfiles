if require("utils").is_plugin_installed("copilot.vim") then
  vim.g.copilot_filetypes = {
    markdown = false,
    xml = false,
  }

  vim.api.nvim_set_keymap("i", "<C-j>", 'copilot#Accept("<CR>")', { expr = true, silent = true })
  vim.keymap.set('i', '<C-i>', '<Plug>(copilot-next)', {})
  vim.keymap.set('i', '<C-k>', '<Plug>(copilot-previous)', {})
  vim.keymap.set('i', '<M-]>', '<Plug>(copilot-dismiss)', {})
  vim.g.copilot_no_tab_map = true
end
