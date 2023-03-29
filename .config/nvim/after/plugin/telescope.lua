local actions = require('telescope.actions')

require('telescope').setup({
    defaults = {
        mappings = {
            i = {
                -- close telescope with a single ESC
                ['<esc>'] = actions.close,
            },
        },
    },
})

