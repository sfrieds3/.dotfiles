return {
  { "nvim-lua/plenary.nvim", lazy = true },
  {
    "ibhagwan/fzf-lua",
    cmd = "FzfLua",
    dependencies = "nvim-tree/nvim-web-devicons",
  },
  { "tami5/sqlite.lua", event = "VeryLazy", module = "sqlite" },
  { "AndrewRadev/linediff.vim", cmd = { "LinediffAdd" } },
  { "folke/neodev.nvim", ft = "lua", opts = {} },
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
