return {
  "nvim-treesitter/nvim-treesitter",
  branch = "main",
  lazy = false,
  build = ":TSUpdate",
  config = function()
    local ensure_installed = {
      "vimdoc",
      "javascript",
      "typescript",
      "c",
      "lua",
      "rust",
      "html",
      "css",
    }

    require("nvim-treesitter").setup({
      install_dir = vim.fn.stdpath("data") .. "/site",
    })

    require("nvim-treesitter").install(ensure_installed)

    vim.treesitter.language.register("html", "htmldjango")

    local start_filetypes = vim.deepcopy(ensure_installed)
    table.insert(start_filetypes, "htmldjango")

    vim.api.nvim_create_autocmd("FileType", {
      pattern = start_filetypes,
      callback = function(args)
        pcall(vim.treesitter.start, args.buf)
      end,
    })
  end,
}
