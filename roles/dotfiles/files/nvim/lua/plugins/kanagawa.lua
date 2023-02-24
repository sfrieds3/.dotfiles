local M = { "rebelot/kanagawa.nvim", lazy = true }

function M.config()
  vim.opt.fillchars:append({
    horiz = "━",
    horizup = "┻",
    horizdown = "┳",
    vert = "┃",
    vertleft = "┨",
    vertright = "┣",
    verthoriz = "╋",
  })
  require("kanagawa").setup({
    dimInactive = true,
    globalStatus = true,
    terminalColors = true,
  })
end

return M
