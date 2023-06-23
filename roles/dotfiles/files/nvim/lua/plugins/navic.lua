local M = {
  "SmiteshP/nvim-navic",
  event = "VeryLazy",
}

function M.config()
  vim.g.navic_silence = true
  require("nvim-navic").setup({ separator = " ", highlight = true, depth_limit = 5 })
end

return M
