if require("utils").is_plugin_installed("oil.nvim") then
  require('oil').setup()

  vim.keymap.set("n", "-", require("oil").open, { desc = "Open parent directory" })
end
