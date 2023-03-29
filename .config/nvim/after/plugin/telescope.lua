local actions = require('telescope.actions')
local builtin = require('telescope.builtin')

require('telescope').setup({
    defaults = {
        layout_strategy = 'vertical',
        layout_config = { height = 0.95 },

        mappings = {
            i = {
                -- close telescope with a single ESC
                ['<esc>'] = actions.close,
            },
        },
    },
})
