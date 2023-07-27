if require("utils").is_plugin_installed("LuaSnip") then
  local ls = require("luasnip")
  local types = require("luasnip.util.types")

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

  for _, snippet_path in ipairs(vim.api.nvim_get_runtime_file("lua/snippets/*.lua", true)) do
    loadfile(snippet_path)()
  end

  vim.keymap.set({ "i", "s" }, "<a-p>", function()
    if ls.expand_or_jumpable() then
      ls.expand()
    end
  end)

  vim.keymap.set({ "i", "s" }, "<a-k>", function()
    if ls.jumpable(1) then
      ls.jump(1)
    end
  end)
  vim.keymap.set({ "i", "s" }, "<a-j>", function()
    if ls.jumpable(-1) then
      ls.jump(-1)
    end
  end)

  vim.keymap.set({ "i", "s" }, "<a-l>", function()
    if ls.choice_active() then
      ls.change_choice(1)
    end
  end)
  vim.keymap.set({ "i", "s" }, "<a-h>", function()
    if ls.choice_active() then
      ls.change_choice(-1)
    end
  end)
end
