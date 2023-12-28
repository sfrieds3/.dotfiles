local M = {
  "sfrieds3/pynvenv.nvim",
  ft = "python",
  -- cmd = { "PynvnevWorkonVenv", "PynvenvActivateVenv", "PynvenvActivateVenvAlias", "PynvenvActivateProjectVenv" },
}

function M.config()
  require("pynvenv").setup({
    -- workon_home = "$HOME/.venv",
  })
end

return M
