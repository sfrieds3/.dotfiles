local M = { "rebelot/kanagawa.nvim", lazy = true }

function M.config()
  require("kanagawa").setup({
    dimInactive = true,
  })
end

return M
