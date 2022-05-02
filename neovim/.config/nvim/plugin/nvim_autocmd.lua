-- nvim autocmd group
vim.api.nvim_create_augroup("Neovim", { clear = true })

vim.api.nvim_create_autocmd("InsertEnter", { command = "set nolist", group = "Neovim" })
vim.api.nvim_create_autocmd("InsertLeave", { command = "set list", group = "Neovim" })
vim.api.nvim_create_autocmd("TextYankPost", { callback = function() vim.highlight.on_yank() end, group = "Neovim" })
