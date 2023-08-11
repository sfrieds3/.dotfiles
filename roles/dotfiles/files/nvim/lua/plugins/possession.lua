local M = {
  "jedrzejboczar/possession.nvim",
  event = "VeryLazy",
}

function M.config()
  require("possession").setup({
    autosave = {
      current = true,
      tmp = true,
    },
  })

  vim.keymap.set(
    "n",
    "\\S",
    require("telescope").extensions.possession.list,
    { desc = "[S]essionManager: load session" }
  )
end

return M
