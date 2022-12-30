local M = {
  "sfrieds3/pynvenv.nvim",
}

function M.config()
  require("pynvenv").setup({
    -- default_venv = "$HOME/.venv/venv",
    workon_home = "$HOME/.venv",
  })
end

return M
