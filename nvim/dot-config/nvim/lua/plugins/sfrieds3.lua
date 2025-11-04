return {
  {
    "sfrieds3/easynote.nvim",
    enabled = false,
    opts = {},
  },
  {
    "sfrieds3/pynvenv.nvim",
    enabled = false,
    ft = "python",
    cmd = { "PynvnevWorkonVenv", "PynvenvActivateVenv", "PynvenvActivateVenvAlias", "PynvenvActivateProjectVenv" },
    opts = {
      workon_home = "$HOME/.venv",
    },
  },
  {
    "sfrieds3/pytest_fixtures.nvim",
    enabled = false,
    opts = {},
    dependencies = "nvim-lua/plenary.nvim",
  },
}
