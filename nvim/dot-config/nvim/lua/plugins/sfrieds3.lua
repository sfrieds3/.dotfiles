return {
  {
    "sfrieds3/easynote.nvim",
    opts = {},
  },
  {
    "sfrieds3/pynvenv.nvim",
    ft = "python",
    cmd = { "PynvnevWorkonVenv", "PynvenvActivateVenv", "PynvenvActivateVenvAlias", "PynvenvActivateProjectVenv" },
    opts = {
      workon_home = "$HOME/.venv",
    },
  },
}
