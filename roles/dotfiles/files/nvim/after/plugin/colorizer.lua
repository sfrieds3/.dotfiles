require('colorizer').setup({
  default_options = {
    RGB = false,
  },
})

vim.keymap.set('n', '_C', '<Cmd>ColorizerToggle<CR>')
