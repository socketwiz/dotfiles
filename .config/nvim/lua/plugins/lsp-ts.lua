-- ts-ls.lua
local filetypes = {
  "javascript", "typescript",
  "javascriptreact", "typescriptreact",
}

return {
  "neovim/nvim-lspconfig",
  ft = filetypes,

  config = function()
    local util = require("lspconfig.util")

    vim.lsp.config("ts_ls", {
      cmd = { "typescript-language-server", "--stdio" },
      filetypes = filetypes,
      root_dir = function(fname)
        local filepath = type(fname) == "string"
          and fname
          or vim.api.nvim_buf_get_name(fname)

        return util.search_ancestors(filepath, function(dir)
          if util.path.exists(util.path.join(dir, "package.json")) then
            return dir
          end
        end) or util.find_git_ancestor(filepath) or vim.loop.os_homedir()
      end,
    })

    vim.lsp.enable("ts_ls")
  end,
}

