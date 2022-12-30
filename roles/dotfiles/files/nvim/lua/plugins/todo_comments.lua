local M = {
  "folke/todo-comments.nvim",
  dependencies = "nvim-lua/plenary.nvim",
}

function M.config()
  require("todo-comments").setup({})
  vim.keymap.set("n", "]T", function()
    require("todo-comments").jump_next()
  end, { desc = "Next todo comment" })

  vim.keymap.set("n", "[T", function()
    require("todo-comments").jump_prev()
  end, { desc = "Previous todo comment" })
  vim.keymap.set("n", "<Leader>vt", "<Cmd>TodoTelescope<CR>")
end

return M
