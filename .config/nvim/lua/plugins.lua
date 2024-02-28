return require("lazy").setup({
	{
		-- Fuzzy finder
		"nvim-telescope/telescope.nvim",
		tag = "0.1.4",
		-- or                            , branch = '0.1.x',
		dependencies = { { "nvim-lua/plenary.nvim" } },
	},
	"tpope/vim-surround", -- Quote, paranthesis wrapper
	"tpope/vim-fugitive", -- Git integration
	"tomtom/tcomment_vim", -- Commenter
	"mbbill/undotree", -- Undo manager
	"sbdchd/neoformat", -- Prettier
	"github/copilot.vim", -- github AI code completion

	-- Replaces UI messages, cmdline and the popupmenu
	{
		"folke/noice.nvim",
		dependencies = {
			"MunifTanjim/nui.nvim",
			"rcarriga/nvim-notify",
		},
	},

	-- File explorer
	"stevearc/oil.nvim",

	{
		-- Fancy status line
		"nvim-lualine/lualine.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons", opt = true },
	},
	{ -- comments
		"numToStr/Comment.nvim",
	},

	{ -- Theme
		"folke/tokyonight.nvim",
		lazy = false,
		priority = 1000,
		opts = {},
	},

	{
		-- Diagnostics (linter errors and such)
		"folke/trouble.nvim",
		dependencies = "nvim-tree/nvim-web-devicons",
		opts = {},
	},

	-- Git decorations in the gutter
	{ "lewis6991/gitsigns.nvim" },

	-- Honor .editorconfig file
	"gpanders/editorconfig.nvim",

	-- Find a .git upstream and make that the root
	"notjedi/nvim-rooter.lua",

	-- Rust support
	"simrat39/rust-tools.nvim",

	-- Syntax highlighting
	{ "nvim-treesitter/nvim-treesitter" },

	-- LSP with lsp-zero
	{
		{
			"VonHeikemen/lsp-zero.nvim",
			branch = "v3.x",
			lazy = true,
			config = false,
			init = function()
				-- Disable automatic setup, we are doing it manually
				vim.g.lsp_zero_extend_cmp = 0
				vim.g.lsp_zero_extend_lspconfig = 0
			end,
		},
		{
			"williamboman/mason.nvim",
			lazy = false,
			config = true,
		},

		-- Autocompletion
		{
			"hrsh7th/nvim-cmp",
			event = "InsertEnter",
			dependencies = {
				{ "L3MON4D3/LuaSnip" },
			},
			config = function()
				-- Here is where you configure the autocompletion settings.
				local lsp_zero = require("lsp-zero")
				lsp_zero.extend_cmp()

				-- And you can configure cmp even more, if you want to.
				local cmp = require("cmp")
				local cmp_action = lsp_zero.cmp_action()

				cmp.setup({
					formatting = lsp_zero.cmp_format(),
					mapping = cmp.mapping.preset.insert({
						["<C-Space>"] = cmp.mapping.complete(),
						["<C-u>"] = cmp.mapping.scroll_docs(-4),
						["<C-d>"] = cmp.mapping.scroll_docs(4),
						["<C-f>"] = cmp_action.luasnip_jump_forward(),
						["<C-b>"] = cmp_action.luasnip_jump_backward(),
					}),
				})
			end,
		},

		-- LSP
		{
			"neovim/nvim-lspconfig",
			cmd = { "LspInfo", "LspInstall", "LspStart" },
			event = { "BufReadPre", "BufNewFile" },
			dependencies = {
				{ "hrsh7th/cmp-nvim-lsp" },
				{ "williamboman/mason-lspconfig.nvim" },
			},
			config = function()
				-- This is where all the LSP shenanigans will live
				local lsp_zero = require("lsp-zero")
				lsp_zero.extend_lspconfig()

				lsp_zero.on_attach(function(client, bufnr)
					-- see :help lsp-zero-keybindings
					-- to learn the available actions
					lsp_zero.default_keymaps({ buffer = bufnr })
				end)

				require("mason-lspconfig").setup({
					ensure_installed = {},
					handlers = {
						lsp_zero.default_setup,
						lua_ls = function()
							-- (Optional) Configure lua language server for neovim
							local lua_opts = lsp_zero.nvim_lua_ls()
							require("lspconfig").lua_ls.setup(lua_opts)
						end,
					},
				})
			end,
		},
	},
})
