vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = { "*.lua", "*.ts", "*.js", "*.py", "*.json", "*.css", "*.scss" },
  callback = function(args)
    local ok, conform = pcall(require, "conform")
    if ok then
      conform.format({ bufnr = args.buf })
    end
  end,
})
