local M = {}

M.is_plugin_installed = function(plugin_name)
  local lazy_dir = vim.fn.stdpath('data') .. '/lazy/'
  local plugin_path = lazy_dir .. plugin_name

  return vim.fn.isdirectory(plugin_path) == 1
end

return M
