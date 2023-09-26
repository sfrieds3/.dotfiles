return {
  "tami5/sqlite.lua",
  "rcarriga/nvim-notify",
  "romainl/vim-qlist",
  "tpope/vim-sleuth",
  "wellle/targets.vim",
  "stevearc/overseer.nvim",
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
    {
      "utilyre/barbecue.nvim",
      name = "barbecue",
      version = "*",
      dependencies = {
        "SmiteshP/nvim-navic",
        "nvim-tree/nvim-web-devicons",
      },
      opts = {
        -- configurations go here
      },
    },
  },
  { "tpope/vim-fugitive", cmd = "Git", ft = "gitcommit" },
  { "rhysd/git-messenger.vim", cmd = { "GitMessenger" } },
  { "akinsho/git-conflict.nvim", version = "*", config = true },
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
  { "sainnhe/edge" },
  { "nyoom-engineering/oxocarbon.nvim" },
  { "briones-gabriel/darcula-solid.nvim", dependencies = { "rktjmp/lush.nvim" } },
  { "nordtheme/vim", name = "nord" },
  { "loctvl842/monokai-pro.nvim" },
  { "Domeee/mosel.nvim" },
}
