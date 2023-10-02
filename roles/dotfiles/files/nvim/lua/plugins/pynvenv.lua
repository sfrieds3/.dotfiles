local M = {
  "sfrieds3/pynvenv.nvim",
  -- cmd = { "PynvnevWorkonVenv", "PynvenvActivateVenv", "PynvenvActivateVenvAlias", "PynvenvActivateProjectVenv" },
}

function M.config()
  require("pynvenv").setup({
    -- workon_home = "$HOME/.venv",
  })
end

return M
