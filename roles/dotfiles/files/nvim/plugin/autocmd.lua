vim.api.nvim_create_autocmd({ 'CursorMoved', 'BufWinEnter', 'BufFilePost', 'InsertEnter', 'BufWritePost' }, {
  callback = function()
    require('winbar').get_winbar()
  end,
})
