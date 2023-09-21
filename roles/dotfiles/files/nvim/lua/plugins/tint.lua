local M = {
  "levouh/tint.nvim",
}

function M.config()
  require("tint").setup({
    tint = -33,
    saturation = 0.6,
  })
end

return M
