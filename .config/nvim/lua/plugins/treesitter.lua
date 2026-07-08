return {
  -- LazyVim configures nvim-treesitter (main branch) itself: highlighting,
  -- indent, folds, and parser installation. We only extend its parser list
  -- (opts_extend appends `ensure_installed`) and register the htmldjango alias.
  "nvim-treesitter/nvim-treesitter",
  opts = {
    ensure_installed = { "rust", "css" },
  },
  init = function()
    vim.treesitter.language.register("html", "htmldjango")
  end,
}
