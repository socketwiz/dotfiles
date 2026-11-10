return {
  -- LazyVim's default completion engine (replaces the old nvim-cmp setup).
  -- Point it at LuaSnip so our custom snippets in ./snippets keep working.
  "saghen/blink.cmp",
  dependencies = { "L3MON4D3/LuaSnip" },
  opts = {
    snippets = { preset = "luasnip" },
    keymap = {
      -- accept like LazyVim's <C-y>; plain <C-j> (newline) when no menu is open
      ["<C-j>"] = { "select_and_accept", "fallback" },
      -- dismiss moved off the preset's <C-e>, leaving <C-e> free
      ["<C-l>"] = { "cancel", "fallback" },
      ["<C-e>"] = false,
    },
  },
}
