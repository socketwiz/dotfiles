if require("utils").is_plugin_installed("LuaSnip") then
  local ls = require("luasnip")
  local types = require("luasnip.util.types")

  -- enable some builtin html snippets for jsx
  ls.filetype_extend("javascriptreact", { "html" })

  ls.config.set_config({
    history = true,

    updateevents = "TextChanged,TextChangedI",

    enable_autosnippets = true,

    ext_opts = {
      [types.choiceNode] = {
        active = {
          virt_text = { { "<--", "Error" } },
        },
      },
    },
  })

  require("luasnip.loaders.from_lua").load({ paths = { "~/.config/nvim/snippets" } })
  -- for _, snippet_path in ipairs(vim.api.nvim_get_runtime_file("snippets/*.lua", true)) do
  --   loadfile(snippet_path)()
  -- end

  vim.keymap.set({ "i" }, "<A-K>", function()
    ls.expand()
  end, { silent = true })
  vim.keymap.set({ "i", "s" }, "<A-L>", function()
    ls.jump(1)
  end, { silent = true })
  vim.keymap.set({ "i", "s" }, "<A-J>", function()
    ls.jump(-1)
  end, { silent = true })

  vim.keymap.set({ "i", "s" }, "<A-E>", function()
    if ls.choice_active() then
      ls.change_choice(1)
    end
  end, { silent = true })
end
