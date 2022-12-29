local M = { "j-hui/fidget.nvim" }

function M.config()
  require("fidget").setup({
    text = {
      spinner = "bouncing_ball",
    },
  })
end

return M
