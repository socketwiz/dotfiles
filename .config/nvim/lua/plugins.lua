
vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function()
  use 'wbthomason/packer.nvim' -- Package manager
  use 'neovim/nvim-lspconfig' -- Collection of configurations for the built-in LSP client

  -- Parse generator (syntax tree for souce files)
  use {
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate'
  }

  -- Fuzzy finder
  use {
    'nvim-telescope/telescope.nvim',
    requires = {
      'nvim-lua/plenary.nvim'
    }
  }

  -- File explorer
  use {
    'kyazdani42/nvim-tree.lua',
    requires = {
      'kyazdani42/nvim-web-devicons', -- optional, for file icon
    }
  }

  use 'tpope/vim-surround' -- Quote, paranthesis wrapper
  use 'tomtom/tcomment_vim' -- Commenter

  use 'folke/tokyonight.nvim' -- Theme
  use 'folke/which-key.nvim' -- Show which hotkeys are available for use

  use 'dense-analysis/ale'
  use {'neoclide/coc.nvim', branch = 'release'}

  -- Git decorations in the gutter
  use {
    'lewis6991/gitsigns.nvim',
    requires = {
      'nvim-lua/plenary.nvim'
    },
  }

  -- Automatically set up your configuration after cloning packer.nvim
  -- Put this at the end after all plugins
  if packer_bootstrap then
    require('packer').sync()
  end
end)

