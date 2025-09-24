return {
  "neovim/nvim-lspconfig",
  ft = "python",
  name = "pylsp-setup",
  config = function()
    vim.g.python3_host_prog = vim.fn.expand("~/.pyenv/shims/python3")

    vim.lsp.config("pylsp", {
      cmd = { vim.fn.expand("~/.pyenv/shims/pylsp") },
      settings = {
        pylsp = {
          plugins = {
            pycodestyle = { enabled = false },
            pylint = { enabled = true },
            yapf = { enabled = false },
          },
        },
      },
    })

    vim.lsp.enable("pylsp")
  end,
}

