return {
  "rhysd/committia.vim",
  "tami5/sqlite.lua",
  "rcarriga/nvim-notify",
  "romainl/vim-qlist",
  "tpope/vim-sleuth",
  "wellle/targets.vim",
  "jose-elias-alvarez/typescript.nvim",
  "kyazdani42/nvim-web-devicons",

  {
    "williamboman/mason.nvim",
    cmd = "Mason",
    config = { providers = { "mason.providers.client", "mason.providers.registry-api" } },
    dependencies = {
      "williamboman/mason-lspconfig.nvim",
    },
  },
  {
    "smjonas/inc-rename.nvim",
    cmd = "IncRename",
    config = true,
  },
  {
    "SmiteshP/nvim-navic",
    config = function()
      vim.g.navic_silence = true
      require("nvim-navic").setup({ separator = " ", highlight = true, depth_limit = 5 })
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
    config = { snippet_engine = "luasnip" },
  },
  { "tpope/vim-fugitive", cmd = "Git" },
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
  { "fatih/vim-go", ft = "go", enabled = false },
  {
    "rust-lang/rust.vim",
    ft = "rust",
    config = function()
      vim.g.rustfmt_autosave = 1
    end,
  },
  { "tpope/vim-rails", ft = { "ruby", "eruby" } },
  { "sainnhe/edge", lazy = true },
  { "sainnhe/everforest", lazy = true },
  { "navarasu/onedark.nvim", lazy = true },
  { "catppuccin/nvim", name = "catppuccin", lazy = true },
  { "rose-pine/neovim", name = "rose-pine" },
}
