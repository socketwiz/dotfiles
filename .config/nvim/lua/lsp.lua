
local util = require 'lspconfig/util'

local opts = { noremap=true, silent=true }

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<cr>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gd', '<cmd>lua vim.lsp.buf.definition()<cr>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<cr>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<cr>', opts)
end

-- Use a loop to conveniently call 'setup' on multiple servers and
-- map buffer local keybindings when the language server attaches
-- Dependencies:
-- npm install -g neovim
-- pip install neovim
local servers = {
  'bashls',        -- npm i -g bash-language-server
  'cssls',         -- npm i -g vscode-langservers-extracted
  'denols',        -- cargo install deno
  'eslint',        -- npm i -g vscode-langservers-extracted
  'html',          -- npm i -g vscode-langservers-extracted
  'jsonls',        -- npm i -g vscode-langservers-extracted
  'rust_analyzer', -- https://rust-analyzer.github.io/manual.html#rust-analyzer-language-server-binary
}
for _, lsp in pairs(servers) do
  capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
  cmd = {}
  filetypes = {}
  handlers = {}
  init_options = {}
  root_pattern = {}
  settings = {}

  if lsp == 'bashls' then
    cmd = { 'bash-language-server', 'start' }
    cmd_env = {
      GLOB_PATTERN = '*@(.sh|.inc|.bash|.command)'
    }
    filetypes = { 'sh' }
    root_dir = find_git_ancestor
    single_file_support = true
  elseif lsp == 'cssls' then
    cmd = { 'vscode-css-language-server', '--stdio' }
    filetypes = { 'css', 'scss' }
    root_dir = util.root_pattern('package.json', '.git') or bufdir
    settings = {
      css = {
        validate = true
      },
      scss = {
        validate = true
      }
    }
    single_file_support = true
  elseif lsp == 'denols' then
    capabilities = capabilities
    cmd = { 'deno', 'lsp' }
    filetypes = { 'javascript', 'javascriptreact', 'javascript.jsx', 'typescript', 'typescriptreact', 'typescript.tsx' }
    root_dir = util.root_pattern('deno.json', 'deno.jsonc', 'jsconfig.json', 'tsconfig.json', '.git')
    settings = {
      deno = {
        enable = true,
        lint = false
      }
    }
  elseif lsp == 'eslint' then
    capabilities = capabilities
    cmd = { 'vscode-eslint-language-server', '--stdio' }
    filetypes = { 'javascript', 'javascriptreact', 'javascript.jsx', 'typescript', 'typescriptreact', 'typescript.tsx' }
    settings = {
      packageManager = 'npm',
    }
  elseif lsp == 'html' then
    cmd = { 'vscode-html-language-server', '--stdio' }
    filetypes = { 'html' }
    init_options = {
      configurationSection = { 'html', 'css', 'javascript' },
      embeddedLanguages = {
        css = true,
        javascript = true
      },
      provideFormatter = true
    }
    root_dir = function(startpath)
      return M.search_ancestors(startpath, matcher)
    end
    single_file_support = true
  elseif lsp == 'jsonls' then
    capabilities = capabilities
    cmd = { 'vscode-json-language-server', '--stdio' }
    filetypes = { 'json', 'jsonc' }
    init_options = {
      provideFormatter = true
    }
    root_dir = util.find_git_ancestor
    single_file_support = true
  elseif lsp == 'rust_analyzer' then
    capabilities = capabilities
    cmd = { 'rust-analyzer' }
    filetypes = { 'rust' }
    root_dir = util.root_pattern('Cargo.toml', 'rust-project.json')
    settings = {
      ['rust-analyzer'] = {}
    }
  end

  require('lspconfig')[lsp].setup {
    cmd = cmd,
    filetypes = filetypes,
    handlers = handlers,
    init_options = init_options,
    on_attach = on_attach,
    root_dir = root_dir,
    settings = settings,
    single_file_support = single_file_support
  }
end

local dap = require('dap')
dap.adapters.node2 = {
  type = 'executable',
  command = 'node',
  args = { '/usr/local/src/vscode-node-debug2/out/src/nodeDebug.js' },
  name = 'node2'
}
dap.adapters.lldb = {
  type = 'executable',
  command = '/bin/lldb-vscode-13', -- adjust as needed
  name = 'lldb'
}

dap.configurations.javascript = {
  {
    name = 'Launch',
    type = 'node2',
    request = 'launch',
    program = '${file}',
    cwd = vim.fn.getcwd(),
    sourceMaps = true,
    protocol = 'inspector',
    console = 'integratedTerminal',
  },
  {
    -- For this to work you need to make sure the node process is started with the `--inspect` flag.
    name = 'Attach to process',
    type = 'node2',
    request = 'attach',
    processId = require'dap.utils'.pick_process,
  },
}
dap.configurations.rust = {
  {
    name = 'Launch',
    type = 'lldb',
    request = 'launch',
    program = function()
      return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
    end,
    cwd = '${workspaceFolder}',
    stopOnEntry = false,
    args = {},
    runInTerminal = false,
  },
}

