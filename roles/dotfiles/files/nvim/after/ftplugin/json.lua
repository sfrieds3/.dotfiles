local bufnr = vim.api.nvim_get_current_buf()

vim.bo.shiftwidth = 2
vim.bo.softtabstop = 2
vim.bo.formatprg = "jq"

vim.api.nvim_create_autocmd("BufWritePost", {
  callback = function(args)
    require("conform").format({ bufnr = args.bufnr, lsp_fallback = true })
  end,
  buffer = bufnr,
  group = vim.api.nvim_create_augroup("goformat:" .. bufnr, {}),
})
