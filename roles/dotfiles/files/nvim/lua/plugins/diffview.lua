local M = {
  "sindrets/diffview.nvim",
  dependencies = "nvim-lua/plenary.nvim",
  cmd = {
    "DiffviewOpen",
    "DiffviewClose",
    "DiffviewToggleFiles",
    "DiffviewFocusFiles",
    "DiffViewLog",
    "DiffViewFileHistory",
  },
  keys = { "_Dd", "_Dh", "_Dl" },
}

function M.config()
  require("diffview").setup({})

  vim.keymap.set("n", "<Leader>Dd", "<Cmd>DiffviewOpen<CR>")
  vim.keymap.set("n", "<Leader>Dh", "<Cmd>DiffviewFileHistory<CR>")
  vim.keymap.set("n", "<Leader>Dl", "<Cmd>DiffviewLog<CR>")
end

return M
