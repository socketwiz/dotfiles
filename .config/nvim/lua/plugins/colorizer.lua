return {
  "catgoose/nvim-colorizer.lua",
  event = { "BufReadPre", "BufNewFile" },
  config = function()
    require("colorizer").setup({
      "css",
      "javascript",
      "javascriptreact",
      "typescript",
      "typescriptreact",
      "html",
      "json",
      "lua",
    }, {
      mode = "background",
      css = true,
      tailwind = true,
      rgb_fn = true,
      hsl_fn = true,
      names = true,
      sass = { enable = true },
      RRGGBBAA = false,
      always_update = false,
    })
  end,
}
