vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
  group = vim.api.nvim_create_augroup("sfrieds:jinja-filetype", { clear = true }),
  pattern = { "*.jinja2", "*.tmplt" },
  command = "set filetype=jinja",
})
