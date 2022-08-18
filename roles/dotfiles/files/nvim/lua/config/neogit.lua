require("neogit").setup({
  integrations = { diffview = true },
})

vim.keymap.set("n", "_G", "<Cmd>Neogit<CR>")
