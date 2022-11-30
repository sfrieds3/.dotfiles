require("diffview").setup({})

vim.keymap.set("n", "<Leader>Dd", "<Cmd>DiffviewOpen<CR>")
vim.keymap.set("n", "<Leader>Dh", "<Cmd>DiffviewFileHistory<CR>")
vim.keymap.set("n", "<Leader>Dl", "<Cmd>DiffviewLog<CR>")
