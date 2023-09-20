return {
  "nvim-telescope/telescope.nvim",
  event = "VeryLazy",
  config = function()
    require("plugins.telescope.config").setup()
    require("plugins.telescope.keymap").set_keymap()
  end,
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-lua/popup.nvim",
    "nvim-telescope/telescope-live-grep-args.nvim",
    "nvim-telescope/telescope-ui-select.nvim",
    "nvim-telescope/telescope-file-browser.nvim",
    { "nvim-telescope/telescope-frecency.nvim", dependencies = { "tami5/sqlite.lua" } },
    {
      "nvim-telescope/telescope-fzf-native.nvim",
      build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
    },
    "nvim-tree/nvim-web-devicons",
    "debugloop/telescope-undo.nvim",
  },
}
