return {
  "tami5/sqlite.lua",
  "rcarriga/nvim-notify",
  "romainl/vim-qlist",
  "tpope/vim-sleuth",
  "wellle/targets.vim",
  "stevearc/overseer.nvim",
  "jose-elias-alvarez/typescript.nvim",

  {
    "luukvbaal/statuscol.nvim",
    opts = { setopt = true },
  },
  {
    "smjonas/inc-rename.nvim",
    cmd = "IncRename",
    config = true,
  },
  {
    "danymat/neogen",
    keys = {
      {
        "<leader>cc",
        function()
          require("neogen").generate({})
        end,
        desc = "Neogen Comment",
      },
    },
    opts = { snippet_engine = "luasnip" },
  },
  { "tpope/vim-fugitive", cmd = "Git", ft = "gitcommit" },
  { "rhysd/git-messenger.vim", cmd = { "GitMessenger" } },
  { "AndrewRadev/linediff.vim", cmd = { "LinediffAdd" } },
  { "chrisbra/NrrwRgn", cmd = { "NR", "NarrowRegion" } },
  { "folke/lua-dev.nvim", ft = { "lua" } },
  { "RRethy/nvim-align", cmd = { "Align" } },
  { "romainl/vim-qf", ft = { "qf" } },
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
  { "sainnhe/edge", lazy = true },
  { "sainnhe/everforest", lazy = true },
  { "navarasu/onedark.nvim", lazy = true },
  { "rose-pine/neovim", name = "rose-pine" },
  { "nyoom-engineering/oxocarbon.nvim" },
  { "briones-gabriel/darcula-solid.nvim", dependencies = { "rktjmp/lush.nvim" } },
  { "nordtheme/vim", name = "nord" },
}
