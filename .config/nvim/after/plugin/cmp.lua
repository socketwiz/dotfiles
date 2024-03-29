if require("utils").is_plugin_installed("nvim-cmp") then
  local cmp = require("cmp")
  local luasnip = require("luasnip")

  cmp.setup({
    snippet = {
      expand = function(args)
        luasnip.lsp_expand(args.body)
      end,
    },
    --     mapping = cmp.mapping.preset.insert({
    --       -- ['<Tab>'] = nil,
    --       -- ['<S-Tab>'] = nil,
    --       ["<C-b>"] = cmp.mapping.scroll_docs(-4),
    --       ["<C-f>"] = cmp.mapping.scroll_docs(4),
    --       ["<C-Space>"] = cmp.mapping.complete(),
    --       ["<C-e>"] = cmp.mapping.abort(),
    --       ["<CR>"] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    --     }),
    sources = cmp.config.sources({
      { name = "nvim_lsp", max_item_count = 6 },
      { name = "luasnip" },
    }, {
      { name = "buffer", max_item_count = 6 },
    }),
  })
  --
  --   -- Set configuration for specific filetype.
  --   cmp.setup.filetype("gitcommit", {
  --     sources = cmp.config.sources({
  --       { name = "cmp_git" },
  --     }, {
  --       { name = "buffer" },
  --     }),
  --   })
  --
  --   -- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
  --   cmp.setup.cmdline({ '/', '?' }, {
  --     mapping = cmp.mapping.preset.cmdline(),
  --     sources = {
  --       { name = 'buffer' }
  --     }
  --   })
end
