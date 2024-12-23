vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
  group = vim.api.nvim_create_augroup("SetZshFiletype", { clear = true }),
  pattern = { "*zprofile", "*.zprofile", "*.zshenv", "*.zshrc", "*zshrc" },
  command = "set filetype=zsh",
})
