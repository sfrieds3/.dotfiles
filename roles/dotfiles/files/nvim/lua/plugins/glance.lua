local M = {
  "dnlhc/glance.nvim",
}

function M.config()
  require("glance").setup({
    preview_win_opts = {
      winblend = 10,
    },
    border = {
      enable = true,
    },
    height = 33,
    theme = {
      mode = "auto",
    },
  })
  vim.keymap.set("n", "gD", "<CMD>Glance definitions<CR>", { desc = "Glance: [g]lance [D]efinitions" })
  vim.keymap.set("n", "gR", "<CMD>Glance references<CR>", { desc = "Glance: [g]lance [R]eferences" })
end

return M
