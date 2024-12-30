if require("utils").is_plugin_installed("LuaSnip") then
  local ls = require("luasnip")
  local types = require("luasnip.util.types")

  -- Enable some builtin HTML snippets for JSX
  ls.filetype_extend("javascriptreact", { "html" })

  -- Set LuaSnip configuration
  ls.config.set_config({
    history = true, -- Enable snippet history
    updateevents = "TextChanged,TextChangedI", -- Update snippets as you type
    enable_autosnippets = true, -- Allow automatic snippets

    -- Visual settings for choice nodes
    ext_opts = {
      [types.choiceNode] = {
        active = {
          virt_text = { { "<--", "Error" } },
        },
      },
    },

    -- Enable jsregexp transformations for placeholders/variables
    enable_jsregexp = true,
  })

  -- Load snippets from custom paths
  require("luasnip.loaders.from_lua").load({ paths = { "~/.config/nvim/snippets" } })

  -- Key mappings for snippet operations
  vim.keymap.set({ "i" }, "<A-K>", function()
    ls.expand()
  end, { silent = true })
  vim.keymap.set({ "i", "s" }, "<A-L>", function()
    ls.jump(1)
  end, { silent = true })
  vim.keymap.set({ "i", "s" }, "<A-J>", function()
    ls.jump(-1)
  end, { silent = true })

  -- Key mapping for cycling through choices
  vim.keymap.set({ "i", "s" }, "<A-E>", function()
    if ls.choice_active() then
      ls.change_choice(1)
    end
  end, { silent = true })
end

