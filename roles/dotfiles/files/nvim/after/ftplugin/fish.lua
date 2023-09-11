local bufnr = vim.api.nvim_get_current_buf()

vim.api.nvim_create_autocmd("BufWritePre", {
  callback = function(args)
    require("conform").format({ formatters = { "fish_indent" }, bufnr = args.bufnr })
  end,
  buffer = bufnr,
  group = vim.api.nvim_create_augroup("fish_indent:" .. bufnr, {}),
})
