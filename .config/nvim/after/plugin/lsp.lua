local lsp = require("lsp-zero").preset({
	name = "recommended",
})

lsp.ensure_installed({
	"eslint",
	"rust_analyzer",
	"tsserver",
})

lsp.format_on_save({
	servers = {
		["eslint"] = { "javascript" },
		["tsserver"] = { "typescript" },
		["rust_analyzer"] = { "rust" },
	},
})

lsp.setup()

local rust_lsp = lsp.build_options('rust_analyzer', {})

require('rust-tools').setup({server = rust_lsp})

