-- nvim autocmd group
vim.api.nvim_create_augroup('Neovim', { clear = true })
vim.api.nvim_create_autocmd('InsertEnter', { command = "set nolist", group = "Neovim" })
vim.api.nvim_create_autocmd('InsertLeave', { command = "set list", group = "Neovim" })
vim.api.nvim_create_autocmd('TextYankPost', { callback = function() vim.highlight.on_yank() end, group = "Neovim" })

vim.api.nvim_create_augroup('Winbar', { clear = true })
vim.api.nvim_create_autocmd({ 'BufWinEnter', 'BufFilePost', 'InsertEnter', 'BufWritePost' }, {
  group = 'Winbar',
  callback = function()
    require('scwfri.winbar').get_winbar()
  end,
})

vim.api.nvim_create_augroup('NvimStartup', { clear = true })
vim.api.nvim_create_autocmd({ 'VimEnter' }, {
  group = 'NvimStartup',
  callback = function()
  end
})
