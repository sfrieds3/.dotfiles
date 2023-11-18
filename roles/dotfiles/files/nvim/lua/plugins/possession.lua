local M = {
  "jedrzejboczar/possession.nvim",
  event = "VeryLazy",
}

function M.config()
  require("possession").setup({
    autosave = {
      current = true,
      tmp = false,
    },
    commands = {
        save = "SSave",
        load = "SLoad",
        delete = "SDelete",
        list = "SList",
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
