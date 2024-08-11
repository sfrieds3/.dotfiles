vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
  group = vim.api.nvim_create_augroup("SetJinjaFiletype", { clear = true }),
  pattern = { "*.jinja2" },
  command = "set filetype=jinja.html",
})
