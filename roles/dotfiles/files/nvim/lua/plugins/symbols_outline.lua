local M = { "simrat39/symbols-outline.nvim" }

function M.config()
  require("symbols-outline").setup({})

  vim.keymap.set("n", "<Leader><CR>", "<Cmd>SymbolsOutline<CR>")
end

return M
