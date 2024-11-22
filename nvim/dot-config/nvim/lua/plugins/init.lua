return {
  { "nvim-lua/plenary.nvim", lazy = true },
  { "tami5/sqlite.lua", event = "VeryLazy", module = "sqlite" },
  {
    "folke/lazydev.nvim",
    ft = "lua",
    opts = {
      library = {
        "luvit-meta/library",
      },
    },
    dependenciew = {
      { "Bilal2453/luvit-meta", lazy = true },
    },
  },
  {
    "tpope/vim-scriptease",
    cmd = {
      "Messages",
      "Verbose",
      "Time",
      "Scriptnames",
    },
  },
  { "chrisbra/csv.vim", ft = "csv" },
}
