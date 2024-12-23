vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
  group = vim.api.nvim_create_augroup("set-todo-ft-markdown", { clear = true }),
  pattern = { "TODO" },
  command = "set filetype=markdown",
})
