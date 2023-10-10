return {
  "tami5/sqlite.lua",
  "tpope/vim-sleuth",
  "wellle/targets.vim",
  {
    "smjonas/inc-rename.nvim",
    cmd = "IncRename",
    config = true,
  },
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
  { "tpope/vim-fugitive", cmd = "Git", ft = "gitcommit" },
  { "rhysd/git-messenger.vim", cmd = { "GitMessenger" } },
  { "akinsho/git-conflict.nvim", version = "*", config = true },
  { "AndrewRadev/linediff.vim", cmd = { "LinediffAdd" } },
  { "folke/lua-dev.nvim", ft = { "lua" } },
  {
    "tpope/vim-scriptease",
    cmd = {
      "Messages",
      "Verbose",
      "Time",
      "Scriptnames",
    },
  },
  { "milisims/nvim-luaref", ft = { "lua" } },
  { "chrisbra/csv.vim", ft = "csv" },
}
