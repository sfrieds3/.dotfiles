return {
  { "nvim-lua/plenary.nvim" },
  {
    "ibhagwan/fzf-lua",
    commands = "FzfLua",
    dependencies = "nvim-tree/nvim-web-devicons",
  },
  { "tami5/sqlite.lua" },
  {
    "danymat/neogen",
    keys = {
      {
        "<Leader>cc",
        function()
          require("neogen").generate({})
        end,
        desc = "Neogen Comment",
      },
    },
    config = function()
      require("neogen").setup({
        snippet_engine = "luasnip",
      })
    end,
  },
  { "AndrewRadev/linediff.vim", cmd = { "LinediffAdd" } },
  { "folke/neodev.nvim", opts = {} },
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
