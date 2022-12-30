local M = { "rebelot/kanagawa.nvim", lazy = false }

function M.config()
  require("kanagawa").setup({
    dimInactive = true,
  })
end

return M
