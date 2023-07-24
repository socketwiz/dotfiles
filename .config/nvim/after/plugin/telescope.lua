if require("utils").is_plugin_installed("telescope.nvim") then
  local actions = require("telescope.actions")

  require("telescope").setup({
    defaults = {
      file_ignore_patterns = { ".git" },
      layout_strategy = "vertical",
      layout_config = { height = 0.95 },
      mappings = {
        i = {
          -- close telescope with a single ESC
          ["<esc>"] = actions.close,
        },
      },
    },
    pickers = {
      find_files = {
        hidden = true,
      }
    }
  })
end
