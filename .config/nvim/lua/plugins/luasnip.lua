return {
  "L3MON4D3/LuaSnip",
  version = "v2.*",
  build = "make install_jsregexp",
  event = "InsertEnter",
  config = function()
    local ls = require("luasnip")
    local types = require("luasnip.util.types")

    -- Enable HTML snippets in JSX/React
    ls.filetype_extend("javascriptreact", { "html" })

    -- Configuration
    ls.config.set_config({
      history = true,
      updateevents = "TextChanged,TextChangedI",
      enable_autosnippets = true,
      enable_jsregexp = true,
      ext_opts = {
        [types.choiceNode] = {
          active = {
            virt_text = { { "<--", "Error" } },
          },
        },
      },
    })

    -- Load custom snippets
    require("luasnip.loaders.from_lua").load({ paths = { "~/.config/nvim/snippets" } })

    -- Keymaps
    vim.keymap.set({ "i" }, "<A-K>", function() ls.expand() end, { silent = true })
    vim.keymap.set({ "i", "s" }, "<A-L>", function() ls.jump(1) end, { silent = true })
    vim.keymap.set({ "i", "s" }, "<A-J>", function() ls.jump(-1) end, { silent = true })
    vim.keymap.set({ "i", "s" }, "<A-E>", function()
      if ls.choice_active() then
        ls.change_choice(1)
      end
    end, { silent = true })
  end,
}

