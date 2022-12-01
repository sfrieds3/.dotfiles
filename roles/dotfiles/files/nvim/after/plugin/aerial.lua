require("aerial").setup({
  min_width = { 20, 0.25 },
})

vim.keymap.set("n", "<Leader><CR>", "<Cmd>AerialToggle<CR>")
