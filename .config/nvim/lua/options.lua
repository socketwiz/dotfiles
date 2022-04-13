
local set = vim.opt  -- set options
local let = vim.g    -- let options

set.autoindent = true
set.backspace= 'indent,eol,start'   -- Allow backspacing over indents, end of lines and start of an insert
set.clipboard = 'unnamedplus'       -- Provide clipboard support
set.cursorline = true               -- Display a line below the line the cursor is on
-- Store swap files in fixed location, not current directory.
set.dir='~/.vimswap//,/var/tmp//,/tmp//,c:\tmp,.'
set.encoding = "utf-8"
set.expandtab = true                -- Use spaces instead of tabs
set.hidden = true                   -- Enable background buffers
set.hlsearch = true
set.ignorecase = true               -- Ignore case
set.joinspaces = false              -- No double spaces with join
set.laststatus = 2                  -- Make sure the last window always has a status line
set.list = true                     -- Show some invisible characters
set.number = true                   -- Show line numbers
set.relativenumber = true           -- Relative line numbers
set.ruler = true                    -- Display the line and column Separated by a comma
set.scrolloff = 3                   -- Lines of context
set.shiftwidth = 2                  -- Size of an indent
set.showcmd = true                  -- Show partial command in the last line of the screen
set.showmode = true                 -- Message on the last line showing mode (insert, replace or visual)
set.smartcase = true                -- Do not ignore case with capitals
set.tabstop = 2                     -- Number of spaces tabs count for
set.termguicolors = true            -- True color support
set.timeoutlen = 200                -- Time before which key appears
-- Store undo files in fixed location, not current directory.
set.undodir='~/.vimundo//,/var/tmp//,/tmp//,c:\tmp,.'
set.undofile = true                 -- Enable undo support
set.visualbell = true               -- Turn the beep into a visual representation rather than a sound

let.mapleader = " "                 -- Change leader key to <Space>
let.mapleaderlocal = " "            -- Change leader key to <Space>
-- Theme
-- Change the "hint" color to the "orange" color, and make the "error" color bright red
let.tokyonight_colors = { hint = "orange", error = "#ff0000" }
let.tokyonight_style = "night"      -- Theme
let.tokyonight_sidebars = { "qf", "vista_kind", "terminal", "packer" }

-- Load the colorscheme
vim.cmd[[colorscheme tokyonight]]

-- Plugin options
require('telescope').setup{
  defaults = {
    -- Default configuration for telescope goes here:
    -- config_key = value,
    mappings = {
      i = {
        -- map actions.which_key to <C-h> (default: <C-/>)
        -- actions.which_key shows the mappings for your picker,
        -- e.g. git_{create, delete, ...}_branch for the git_branches picker
        ["<C-h>"] = "which_key"
      }
    }
  },
  pickers = {
    -- Default configuration for builtin pickers goes here:
    -- picker_name = {
    --   picker_config_key = value,
    --   ...
    -- }
    -- Now the picker_config_key will be applied every time you call this
    -- builtin picker
  },
  extensions = {
    -- Your extension configuration goes here:
    -- extension_name = {
    --   extension_config_key = value,
    -- }
    -- please take a look at the readme of the extension you want to configure
  }
}

require('nvim-treesitter.configs').setup {
  ensure_installed = 'maintained',
  highlight = {
    enable = true,
    custom_captures = {
      -- Highlight the @foo.bar capture group with the "Identifier" highlight group.
    },
    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = false,
  },
  incremental_selection = {
    enable = true,
    keymaps = {
    },
  },
  indent = {
    enable = true
  }
}

local cmp = require('cmp')
cmp.setup {
  mapping = {
    ['<C-b>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
    ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
    ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),
    ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
  },
  formatting = {
    fields = { "kind", "abbr", "menu" },
  },
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'buffer' },
    { name = 'path' },
  })
}

require('gitsigns').setup {
  signs = {
    add          = {hl = 'GitSignsAdd'   , text = '│', numhl='GitSignsAddNr'   , linehl='GitSignsAddLn'},
    change       = {hl = 'GitSignsChange', text = '│', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
    delete       = {hl = 'GitSignsDelete', text = '_', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
    topdelete    = {hl = 'GitSignsDelete', text = '‾', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
    changedelete = {hl = 'GitSignsChange', text = '~', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
  },
  watch_gitdir = {
    interval = 1000,
    follow_files = true
  },
  update_debounce = 100,
}

require('nvim-tree').setup {
  disable_netrw        = true,
  hijack_netrw         = true,
  view = {
    mappings = {
      list = {
      { key = "g",                            action = "refresh" },
      { key = "+",                            action = "create" },
      { key = "x",                            action = "remove" },
      { key = "R",                            action = "rename" },
      { key = "^",                            action = "dir_up" },
      }
    },
  },
}

local nvim_lsp = require('lspconfig')
-- Enable bash analyzer
-- npm i -g bash-language-server
nvim_lsp.bashls.setup {}
-- Enable html/css analyzer
-- npm i -g vscode-langservers-extracted
nvim_lsp.cssls.setup {}
nvim_lsp.html.setup {}
-- Enable rust_analyzer
nvim_lsp.rust_analyzer.setup {}
-- Enable typescript analyzer
nvim_lsp.tsserver.setup {}
