require("nvim-tree").setup({
	disable_netrw = true,
	hijack_netrw = true,
	update_focused_file = {
		enable = true,
		update_cwd = true,
	},
	view = {
		mappings = {
			list = {
				{ key = "g", action = "refresh" },
				{ key = "+", action = "create" },
				{ key = "x", action = "remove" },
				{ key = "R", action = "rename" },
				{ key = "^", action = "dir_up" },
			},
		},
	},
})
