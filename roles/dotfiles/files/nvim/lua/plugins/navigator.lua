local M = { "numToStr/Navigator.nvim" }

function M.config()
  -- Configuration
  require("Navigator").setup({})

  -- Keybindings
  vim.keymap.set("n", "<A-h>", "<CMD>NavigatorLeft<CR>")
  vim.keymap.set("n", "<A-l>", "<CMD>NavigatorRight<CR>")
  vim.keymap.set("n", "<A-k>", "<CMD>NavigatorUp<CR>")
  vim.keymap.set("n", "<A-j>", "<CMD>NavigatorDown<CR>")
end

return M