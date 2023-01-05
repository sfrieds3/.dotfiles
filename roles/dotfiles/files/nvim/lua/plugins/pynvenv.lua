local M = {
  "sfrieds3/pynvenv.nvim",
  cmd = { "PynvnevWorkonVenv", "PynvenvActivateVenv", "PynvenvActivateVenvAlias", "PynvenvActivateProjectVenv" },
}

function M.config()
  require("pynvenv").setup({
    workon_home = "$HOME/.venv",
    venv_aliases = {
      rubik = "~/dev/rubik/venv",
    },
  })
end

return M
