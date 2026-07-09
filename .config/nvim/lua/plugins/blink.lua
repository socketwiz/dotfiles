return {
  -- LazyVim's default completion engine (replaces the old nvim-cmp setup).
  -- Point it at LuaSnip so our custom snippets in ./snippets keep working.
  "saghen/blink.cmp",
  dependencies = { "L3MON4D3/LuaSnip" },
  opts = {
    snippets = { preset = "luasnip" },
  },
}
