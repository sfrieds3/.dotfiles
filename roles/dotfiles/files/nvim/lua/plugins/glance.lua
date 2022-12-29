local M = {
  "dnlhc/glance.nvim",
}

function M.config()
  require("glance").setup({
    height = 33,
    theme = {
      mode = "brighten",
    },
  })
  vim.keymap.set("n", "gD", "<CMD>Glance definitions<CR>", { desc = "Glance: [g]lance [D]efinitions" })
  vim.keymap.set("n", "gR", "<CMD>Glance references<CR>", { desc = "Glance: [g]lance [R]eferences" })
end

return M
