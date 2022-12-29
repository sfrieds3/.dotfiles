local M = {
  "folke/tokyonight.nvim",
  lazy = false,
  priority = 999,
}

function M.config()
  local tokyonight = require("tokyonight").setup({
    style = "night",
    dim_inactive = true,
    sidebars = {},
    styles = {
      comments = { italic = true },
      keywords = { italic = true },
      functions = {},
      variables = {},
      sidebars = "normal",
      floats = "transparent",
    },
  })

  -- require("tokyonight").load()
end

return M
