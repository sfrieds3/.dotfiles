require("aerial").setup({
  min_width = { 20, 0.25 },
})

vim.keymap.set("n", "<Leader>a", "<Cmd>AerialToggle<CR>")
