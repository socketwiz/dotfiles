if require("utils").is_plugin_installed("trouble.nvim") then
  vim.keymap.set('n', '<leader>xq', '<cmd>TroubleToggle quickfix<cr>',
    { silent = true, noremap = true }
  )
end
