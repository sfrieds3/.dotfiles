local bufnr = vim.api.nvim_get_current_buf()

vim.bo.shiftwidth = 2
vim.bo.softtabstop = 8

vim.api.nvim_create_autocmd("BufWritePost", {
  callback = function(args)
    require("conform").format({ formatters = { "gofmt", "goimports" }, bufnr = args.bufnr })
  end,
  buffer = bufnr,
  group = vim.api.nvim_create_augroup("goformat:" .. bufnr, {}),
})
