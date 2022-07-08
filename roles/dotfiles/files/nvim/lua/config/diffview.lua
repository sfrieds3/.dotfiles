require('diffview').setup({})

vim.keymap.set('n', '_Dd', '<Cmd>DiffviewOpen<CR>')
vim.keymap.set('n', '_Dh', '<Cmd>DiffviewFileHistory<CR>')
vim.keymap.set('n', '_Dl', '<Cmd>DiffviewLog<CR>')

