local M = { "rebelot/kanagawa.nvim" }

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
    compile = true,
    dimInactive = true,
    globalStatus = true,
    terminalColors = true,
    background = {
      dark = "dragon",
      light = "wave",
    },
  })
end

return M
