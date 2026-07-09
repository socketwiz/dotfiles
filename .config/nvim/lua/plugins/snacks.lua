return {
  -- snacks.nvim is a LazyVim core plugin. Add our finder keymaps on top of
  -- LazyVim's snacks.picker defaults (dashboard stays at LazyVim's default).
  "folke/snacks.nvim",
  keys = {
    { "<C-p>", function() Snacks.picker.git_files() end, desc = "Find Git Files" },
    { "<leader>ff", function() Snacks.picker.files() end, desc = "Find Files" },
    { "<leader>fb", function() Snacks.picker.buffers() end, desc = "Buffers" },
    { "<leader>fr", function() Snacks.picker.recent() end, desc = "Recent Files" },
    { "<leader>fg", function() Snacks.picker.grep_word() end, desc = "Grep Word", mode = { "n", "x" } },
    { "<leader>fk", function() Snacks.picker.keymaps() end, desc = "Keymaps" },
    { "<leader>h", function() Snacks.picker.help() end, desc = "Help Pages" },
  },
}
