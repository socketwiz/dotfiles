
vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function()
	use 'wbthomason/packer.nvim' -- Package manager
	use 'neovim/nvim-lspconfig' -- Collection of configurations for the built-in LSP client

	-- parse generator (syntax tree for souce files)
	use {
		'nvim-treesitter/nvim-treesitter',
		run = ':TSUpdate'
	}

	-- Manage installation of LSP servers
	use 'williamboman/nvim-lsp-installer'

	-- Fuzzy finder
	use {
		'nvim-telescope/telescope.nvim',
		requires = { {'nvim-lua/plenary.nvim'} }
	}

	use 'vim-syntastic/syntastic' -- Syntax checker

	use 'tpope/vim-surround' -- Quote, paranthesis wrapper
	use 'tomtom/tcomment_vim' -- Commenter

	-- Automatically set up your configuration after cloning packer.nvim
	-- Put this at the end after all plugins
	if packer_bootstrap then
		require('packer').sync()
	end
end)

