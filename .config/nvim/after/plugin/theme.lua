-- Change the "hint" color to the "orange" color, and make the "error" color bright red
vim.g.tokyonight_colors = { hint = "orange", error = "#ff0000" }
vim.g.tokyonight_style = "night"
vim.g.tokyonight_sidebars = { "qf", "vista_kind", "terminal", "packer" }
vim.g.UltiSnipsSnippetDirectories = { os.getenv("HOME") .. '/.config/nvim/ultisnips' }

-- Load the colorscheme
vim.cmd[[colorscheme tokyonight]]

