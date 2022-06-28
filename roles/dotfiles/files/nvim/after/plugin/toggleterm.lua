require('toggleterm').setup({
  open_mapping = [[<C-\>]],
  hide_numbers = true,
  persist_size = true,
  persist_mode = true,
  shade_terminals = true,
})

vim.api.nvim_create_augroup('ToggleTerm', { clear = true })
vim.api.nvim_create_autocmd({ 'TermOpen' }, {
  group = 'ToggleTerm',
  pattern = 'term://*',
  callback = function()
    local buf_keymap_opts = { buffer = true }
    vim.keymap.set('t', '<esc>', [[<C-\><C-n>]], buf_keymap_opts)
    vim.keymap.set('t', '<M-h>', [[<C-\><C-n><C-w>h]], buf_keymap_opts)
    vim.keymap.set('t', '<M-j>', [[<C-\><C-n><C-w>j]], buf_keymap_opts)
    vim.keymap.set('t', '<M-k>', [[<C-\><C-n><C-w>k]], buf_keymap_opts)
    vim.keymap.set('t', '<M-l>', [[<C-\><C-n><C-w>l]], buf_keymap_opts)

    vim.keymap.set('t', '<S-Left>', function() tmux.resize_left() end, buf_keymap_opts)
    vim.keymap.set('t', '<S-Down>', function() tmux.resize_bottom() end, buf_keymap_opts)
    vim.keymap.set('t', '<S-Up>', function() tmux.resize_top() end, buf_keymap_opts)
    vim.keymap.set('t', '<S-Right>', function() tmux.resize_right() end, buf_keymap_opts)
  end
})
