local tmux = require('tmux')

tmux.setup({
  navigation = {
    cycle_navigation = true,
    enable_default_keybindings = false,
  },
  resize = {
    enable_default_keybindings = false,
  },
})

vim.keymap.set('n', '<M-h>', function() tmux.move_left() end)
vim.keymap.set('n', '<M-j>', function() tmux.move_bottom() end)
vim.keymap.set('n', '<M-k>', function() tmux.move_top() end)
vim.keymap.set('n', '<M-l>', function() tmux.move_right() end)

vim.keymap.set('n', '<S-Left>', function() tmux.resize_left() end)
vim.keymap.set('n', '<S-Down>', function() tmux.resize_bottom() end)
vim.keymap.set('n', '<S-Up>', function() tmux.resize_top() end)
vim.keymap.set('n', '<S-Right>', function() tmux.resize_right() end)
