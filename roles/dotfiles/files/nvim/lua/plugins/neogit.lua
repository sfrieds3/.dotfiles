local M = {
  "TimUntersberger/neogit",
  cmd = { "Neogit" },
  keys = { "_G" },
  dependencies = {
    "nvim-lua/plenary.nvim",
    "sindrets/diffview.nvim",
  },
}
function M.config()
  require("neogit").setup({
    integrations = { diffview = true },
  })

  vim.keymap.set("n", "_G", "<Cmd>Neogit<CR>")
end

return M
