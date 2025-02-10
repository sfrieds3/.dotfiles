local bufnr = vim.api.nvim_get_current_buf()

vim.opt_local.shiftwidth = 2
vim.opt_local.softtabstop = 2
vim.opt_local.formatprg = "jq"
vim.opt_local.commentstring = "// %s"

-- set package.json to sw=4
if vim.fn.expand("%:t") == "package.json" or vim.fn.expand("%:t") == "package-lock.json" then
  vim.opt_local.shiftwidth = 4
  vim.opt_local.softtabstop = 2
end
