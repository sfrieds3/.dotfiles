local M = { "simrat39/symbols-outline.nvim" }

function M.config()
  require("symbols-outline").setup({
    autofold_depth = 2,
    hover_highlighted_item = true,
    show_symbol_details = true,
    winblend = 10,
    keymaps = {
      hover_symbol = "<Leader>e",
    },
  })

  vim.keymap.set("n", "<Leader><CR>", "<Cmd>SymbolsOutline<CR>")
end

return M
