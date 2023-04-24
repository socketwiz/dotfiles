local M = {}

M.is_plugin_installed = function(plugin_name)
  local packer_dir = vim.fn.stdpath('data') .. '/site/pack/packer/start/'
  local plugin_path = packer_dir .. plugin_name

  return vim.fn.isdirectory(plugin_path) == 1
end

return M
