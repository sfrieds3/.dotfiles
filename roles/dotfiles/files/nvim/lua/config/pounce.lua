require("pounce").setup({
  accept_keys = "JFKDLSAHGNUVRBYTMICEOXWPQZ",
  accept_best_key = "<enter>",
  multi_window = true,
  debug = false,
})

vim.keymap.set({ "n", "v" }, "s", "<Cmd>Pounce<CR>")
vim.keymap.set("n", "S", "<Cmd>PounceRepeat<CR>")
vim.keymap.set("o", "gs", "<Cmd>Pounce<CR>")
