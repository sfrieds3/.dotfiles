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
    "nvim-telescope/telescope-frecency.nvim",
    {
      "nvim-telescope/telescope-fzf-native.nvim",
      build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
    },
    "Marskey/telescope-sg",
    "debugloop/telescope-undo.nvim",
    "fdschmidt93/telescope-egrepify.nvim",
  },
}
