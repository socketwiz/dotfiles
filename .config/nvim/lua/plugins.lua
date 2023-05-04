vim.cmd([[packadd packer.nvim]])

return require("packer").startup(function()
  use("wbthomason/packer.nvim") -- Package manager

  -- Fuzzy finder
  use({
    "nvim-telescope/telescope.nvim",
    tag = "0.1.1",
    -- or                            , branch = '0.1.x',
    requires = { { "nvim-lua/plenary.nvim" } },
  })

  -- File explorer
  use({
    "kyazdani42/nvim-tree.lua",
    requires = {
      "kyazdani42/nvim-web-devicons", -- optional, for file icon
    },
  })

  use("tpope/vim-surround")  -- Quote, paranthesis wrapper
  use("tpope/vim-fugitive")  -- Git integration
  use("tomtom/tcomment_vim") -- Commenter
  use("mbbill/undotree")     -- Undo manager
  use("sbdchd/neoformat")    -- Prettier
  use("github/copilot.vim")
  use({
    -- Fancy status line
    "nvim-lualine/lualine.nvim",
    requires = { "nvim-tree/nvim-web-devicons", opt = true },
  })
  use({ -- comments
    "numToStr/Comment.nvim",
  })

  use("folke/tokyonight.nvim") -- Theme
  use({
    -- Diagnostics (linter errors and such)
    "folke/trouble.nvim",
    requires = "nvim-tree/nvim-web-devicons",
  })

  -- Git decorations in the gutter
  use({ "lewis6991/gitsigns.nvim" })

  -- Honor .editorconfig file
  use("gpanders/editorconfig.nvim")

  -- Find a .git upstream and make that the root
  use("notjedi/nvim-rooter.lua")

  -- Rust support
  use("simrat39/rust-tools.nvim")

  -- Syntax highlighting
  use({ "nvim-treesitter/nvim-treesitter" })

  -- LSP with lsp-zero
  use({
    "VonHeikemen/lsp-zero.nvim",
    branch = "v1.x",
    requires = {
      -- LSP Support
      { "neovim/nvim-lspconfig" },             -- Required
      { "williamboman/mason.nvim" },           -- Optional
      { "williamboman/mason-lspconfig.nvim" }, -- Optional

      -- Autocompletion
      { "hrsh7th/nvim-cmp" },         -- Required
      { "hrsh7th/cmp-nvim-lsp" },     -- Required
      { "hrsh7th/cmp-buffer" },       -- Optional
      { "hrsh7th/cmp-path" },         -- Optional
      { "saadparwaiz1/cmp_luasnip" }, -- Optional
      { "hrsh7th/cmp-nvim-lua" },     -- Optional

      -- Snippets
      { "L3MON4D3/LuaSnip" }, -- Required
    },
  })

  -- Automatically set up your configuration after cloning packer.nvim
  -- Put this at the end after all plugins
  if packer_bootstrap then
    require("packer").sync()
  end
end)
