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
    "Mr-LLLLL/interestingwords.nvim",
    config = function()
      require("interestingwords").setup({
        colors = { "#aeee00", "#ff0000", "#0000ff", "#b88823", "#ffa724", "#ff2c4b" },
        search_count = true,
        navigation = true,
        search_key = "<Leader>m",
        cancel_search_key = "<Leader>M",
        color_key = "<Leader>k",
        cancel_color_key = "<Leader>K",
      })
    end,
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
