local is_executable = vim.fn.executable
local bufnr = vim.api.nvim_get_current_buf()

vim.bo.shiftwidth = 2
vim.bo.softtabstop = 2
vim.bo.expandtab = true
vim.opt_local.formatoptions:remove("r")
