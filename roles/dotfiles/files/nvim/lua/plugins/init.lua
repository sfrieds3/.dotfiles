return {
  { "nvim-lua/plenary.nvim", lazy = true },
  {
    "ibhagwan/fzf-lua",
    cmd = "FzfLua",
    dependencies = "nvim-tree/nvim-web-devicons",
  },
  { "tami5/sqlite.lua", event = "VeryLazy", module = "sqlite" },
  { "AndrewRadev/linediff.vim", cmd = { "LinediffAdd" } },
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
