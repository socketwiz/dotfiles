vim.g.mapleader = " " -- Change leader key to <Space>
vim.g.mapleaderlocal = " " -- Change leader key to <Space>

-- Options
vim.opt.autoindent = true
vim.opt.backspace = "indent,eol,start" -- Allow backspacing over indents, end of lines and start of an insert
vim.opt.clipboard = "unnamedplus" -- Provide clipboard support
vim.opt.cursorline = true -- Display a line below the line the cursor is on
-- Store swap files in fixed location, not current directory.
vim.opt.dir = "~/.vimswap//,/var/tmp//,/tmp//,c:\tmp,."
vim.opt.encoding = "utf-8"
vim.opt.expandtab = true -- Use spaces instead of tabs
vim.opt.hidden = true -- Enable background buffers
vim.opt.hlsearch = true
vim.opt.ignorecase = true -- Ignore case
vim.opt.joinspaces = false -- No double spaces with join
vim.opt.laststatus = 2 -- Make sure the last window always has a status line
vim.opt.list = true -- Show some invisible characters
vim.opt.mouse = "a" -- Enable mouse support to make navigating the LSP UI easier
vim.opt.number = true -- Show line numbers
vim.opt.relativenumber = true -- Relative line numbers
vim.opt.ruler = true -- Display the line and column Separated by a comma
vim.opt.scrolloff = 3 -- Lines of context
vim.opt.shiftwidth = 2 -- Size of an indent
vim.opt.showcmd = true -- Show partial command in the last line of the screen
vim.opt.showmode = true -- Message on the last line showing mode (insert, replace or visual)
vim.opt.smartcase = true -- Do not ignore case with capitals
vim.opt.tabstop = 2 -- Number of spaces tabs count for
vim.opt.termguicolors = true -- True color support
-- Store undo files in fixed location, not current directory.
vim.opt.undodir = "~/.vimundo//,/var/tmp//,/tmp//,c:\tmp,."
vim.opt.undofile = true -- Enable undo support
vim.opt.visualbell = true -- Turn the beep into a visual representation rather than a sound

local fmtGrp = vim.api.nvim_create_augroup("fmt", { clear = true })
vim.api.nvim_create_autocmd("BufWritePre", {
	command = "undojoin | Neoformat",
	group = fmtGrp,
})
