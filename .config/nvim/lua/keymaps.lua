vim.keymap.set("n", "<Up>", "<Nop>")
vim.keymap.set("n", "<Down>", "<Nop>")
vim.keymap.set("n", "<Left>", "<Nop>")
vim.keymap.set("n", "<Right>", "<Nop>")
-- Clear the search highlighting
vim.keymap.set("n", "<leader><space>", ":noh<cr>")

-- Nvim tree
vim.keymap.set("n", "<C-x><C-j>", "<cmd>NvimTreeFindFile<cr>")

local cmp = require("cmp")
cmp.setup({
	mapping = {
		["<C-b>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" }),
		["<C-f>"] = cmp.mapping(cmp.mapping.scroll_docs(4), { "i", "c" }),
		["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
		-- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
		["<C-y>"] = cmp.config.disable,
		["<C-e>"] = cmp.mapping({
			i = cmp.mapping.abort(),
			c = cmp.mapping.close(),
		}),
		-- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
		["<CR>"] = cmp.mapping.confirm({ select = true }),
	},
})

local telescope = require("telescope.builtin")
local path_display = {
	path_display = { shorten = 3 },
}
vim.keymap.set("n", "<leader>fb", function()
	telescope.buffers(path_display)
end, {})
vim.keymap.set("n", "<leader>ff", function()
	telescope.find_files(path_display)
end, {})
vim.keymap.set("n", "<C-p>", function()
	telescope.git_files(path_display)
end, {})
vim.keymap.set("n", "<leader>fg", function()
	telescope.grep_string({ search = vim.fn.input("Grep > ") })
end)
vim.keymap.set("n", "<leader>fh", telescope.help_tags, {})
