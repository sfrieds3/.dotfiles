local M = {
  "sainnhe/gruvbox-material",
  lazy = false,
}

function M.config()
  vim.g.gruvbox_material_background = "hard"
  vim.g.gruvbox_material_foreground = "material"
  vim.g.gruvbox_material_statusline_stype = "material"
  vim.g.gruvbox_better_performance = 1
  vim.g.gruvbox_material_dim_inactive_windows = 1
end

return M
