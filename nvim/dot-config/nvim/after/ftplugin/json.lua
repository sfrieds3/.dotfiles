local bufnr = vim.api.nvim_get_current_buf()

vim.bo.shiftwidth = 2
vim.bo.softtabstop = 2
vim.bo.formatprg = "jq"
vim.bo.commentstring = "// %s"
