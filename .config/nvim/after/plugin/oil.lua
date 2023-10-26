if require("utils").is_plugin_installed("oil.nvim") then
  require("oil").setup({
    view_options = {
      show_hidden = true,
    },
  })

  vim.keymap.set("n", "-", require("oil").open, { desc = "Open parent directory" })
end
