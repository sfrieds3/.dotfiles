return {
  "nvim-telescope/telescope.nvim",
  event = "VeryLazy",
  config = function()
    require("plugins.telescope.config").setup()
  end,
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-lua/popup.nvim",
    "nvim-telescope/telescope-live-grep-args.nvim",
    "nvim-telescope/telescope-ui-select.nvim",
    "nvim-telescope/telescope-project.nvim",
    "nvim-telescope/telescope-file-browser.nvim",
    { "nvim-telescope/telescope-frecency.nvim", dependencies = { "tami5/sqlite.lua" } },
    { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
  },
}
