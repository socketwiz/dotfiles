if require("utils").is_plugin_installed("ChatGPT.nvim") then
  require("chatgpt").setup({
    api_key_cmd = "op read op://Private/chatgpt.neovim/password"
  })
end
