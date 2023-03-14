
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
set.mouse = 'a'                     -- Enable mouse support to make navigating the LSP UI easier
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

let.markdown_fenced_languages = {   -- Deno (JavaScript) language server
  "javascript",
  "js=javascript",
  "ts=typescript"
}
let.mapleader = " "                 -- Change leader key to <Space>
let.mapleaderlocal = " "            -- Change leader key to <Space>
-- Theme
-- Change the "hint" color to the "orange" color, and make the "error" color bright red
let.tokyonight_colors = { hint = "orange", error = "#ff0000" }
let.tokyonight_style = "night"
let.tokyonight_sidebars = { "qf", "vista_kind", "terminal", "packer" }
let.UltiSnipsSnippetDirectories = { os.getenv("HOME") .. '/.config/nvim/ultisnips' }

-- Load the colorscheme
vim.cmd[[colorscheme tokyonight]]

-- Plugin options

-- CtrlP
vim.cmd([[
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.un~,.DS_Store,tags,.*
set wildignore+=*.pyc
let g:ctrlp_custom_ignore = '\.git$\|\.hg$\|\.svn$'
let g:ctrlp_custom_ignore = 'node_modules$\|bower_components$\|public/lib$\|build$\|third-party$'
" 0) don't manage working directory
" 1) the directory of the current file
" 2) the nearest ancestor that contains one of these directories or files:
" .git/ .hg/ .svn/ .bzr/ _darcs/
let g:ctrlp_working_path_mode = 1
" increase the height of the match window
let g:ctrlp_max_height = 30
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . -co --exclude-standard', 'find %s -type f']
let g:ctrlp_use_caching = 0
]])

require('nvim-treesitter.configs').setup {
  ensure_installed = { "rust", "javascript" },
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

-- require('gitsigns').setup {
--   signs = {
--     add          = {hl = 'GitSignsAdd'   , text = '│', numhl='GitSignsAddNr'   , linehl='GitSignsAddLn'},
--     change       = {hl = 'GitSignsChange', text = '│', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
--     delete       = {hl = 'GitSignsDelete', text = '_', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
--     topdelete    = {hl = 'GitSignsDelete', text = '‾', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
--     changedelete = {hl = 'GitSignsChange', text = '~', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
--   },
--   watch_gitdir = {
--     interval = 1000,
--     follow_files = true
--   },
--   update_debounce = 100,
-- }

require('nvim-tree').setup {
  disable_netrw = true,
  hijack_netrw  = true,
  update_focused_file = {
    enable = true,
    update_cwd  = true
  },
  view = {
    mappings = {
      list = {
      { key = "g", action = "refresh" },
      { key = "+", action = "create" },
      { key = "x", action = "remove" },
      { key = "R", action = "rename" },
      { key = "^", action = "dir_up" },
      }
    },
  },
}

local cmp = require('cmp')

cmp.setup {
  snippet = {
    -- REQUIRED - you must specify a snippet engine
    expand = function(args)
      vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
    end,
  },
  sources = {
    { name = 'buffer' },
    { name = 'cmdline' },
    { name = 'nvim_lsp' },
    { name = 'path' },
    { name = 'ultisnips' }
  },
  filetype = {
  }
}
cmp.setup.filetype('gitcommit', {
    sources = cmp.config.sources({
      { name = 'cmp_git' }, -- You can specify the `cmp_git` source if you were installed it.
    }, {
      { name = 'buffer' },
    })
  })

require('dapui').setup()

require('nvim-rooter').setup {
  rooter_patterns = { '.git' },
  trigger_patterns = { '*' },
  manual = false,
}

