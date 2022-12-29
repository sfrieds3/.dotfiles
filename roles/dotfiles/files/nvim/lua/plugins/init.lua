return {
  "rhysd/committia.vim",
  "tami5/sqlite.lua",
  "rcarriga/nvim-notify",
  "romainl/vim-qlist",
  "tpope/vim-sleuth",
  "wellle/targets.vim",
  "jose-elias-alvarez/typescript.nvim",

  { "tpope/vim-fugitive", cmd = "Git" },
  { "rhysd/git-messenger.vim", cmd = { "GitMessenger" } },
  { "AndrewRadev/linediff.vim", cmd = { "LinediffAdd" } },
  { "chrisbra/NrrwRgn", cmd = { "NR", "NarrowRegion" } },
  { "folke/lua-dev.nvim", ft = { "lua" } },
  { "RRethy/nvim-align", cmd = { "Align" } },
  {
    "ludovicchabant/vim-gutentags",
    config = function()
      vim.g.gutentags_cache_dir = vim.env.XDG_CACHE_HOME .. "/tags"
      if vim.fn.has("macunix") then
        vim.g.gutentags_ctags_executable = "/opt/homebrew/bin/ctags"
      end
    end,
  },
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
  {
    "andymass/vim-matchup",
    config = function()
      vim.g.matchup_matchparen_offscreen = { method = "popup" }
      vim.g.matchup_matchparen_deferred_show_delay = 500
      vim.g.matchup_matchparen_deferred_hide_delay = 500
      vim.g.matchup_matchparen_timeout = 100
      vim.g.matchup_matchparen_deferred = 1
    end,
  },
  { "milisims/nvim-luaref", ft = { "lua" } },

  {
    "dnlhc/glance.nvim",
    config = function()
      require("glance").setup({
        height = 33,
        theme = {
          mode = "brighten",
        },
      })
      vim.keymap.set("n", "gD", "<CMD>Glance definitions<CR>", { desc = "Glance: [g]lance [D]efinitions" })
      vim.keymap.set("n", "gR", "<CMD>Glance references<CR>", { desc = "Glance: [g]lance [R]eferences" })
    end,
  },
  {
    "folke/trouble.nvim",
    dependencies = "kyazdani42/nvim-web-devicons",
    cmd = { "Trouble", "TroubleToggle", "TroubleRefresh" },
    config = function()
      require("trouble").setup({})
    end,
  },
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

  {
    "NTBBloodbath/doom-one.nvim",
    init = function()
      -- Add color to cursor
      vim.g.doom_one_cursor_coloring = false
      -- Set :terminal colors
      vim.g.doom_one_terminal_colors = true
      -- Enable italic comments
      vim.g.doom_one_italic_comments = true
      -- Enable TS support
      vim.g.doom_one_enable_treesitter = true
      -- Color whole diagnostic text or only underline
      vim.g.doom_one_diagnostics_text_color = false
      -- Enable transparent background
      vim.g.doom_one_transparent_background = false

      -- Pumblend transparency
      vim.g.doom_one_pumblend_enable = false
      vim.g.doom_one_pumblend_transparency = 20

      -- Plugins integration
      vim.g.doom_one_plugin_telescope = true
      vim.g.doom_one_plugin_neogit = true
      vim.g.doom_one_plugin_nvim_tree = true
      vim.g.doom_one_plugin_vim_illuminate = true
      vim.g.doom_one_plugin_neo_tree = true

      -- Pumblend transparency
      vim.g.doom_one_pumblend_enable = true
      vim.g.doom_one_pumblend_transparency = 20
    end,
  },
  { "sainnhe/edge", lazy = true },
  { "sainnhe/gruvbox-material", lazy = true },
  {
    "sainnhe/sonokai",
    lazy = true,
    init = function()
      vim.g.sonokai_style = "espresso"
      vim.g.sonokai_dim_inactive_windows = true
      vim.g.sonokai_better_performance = true
    end,
  },
  { "navarasu/onedark.nvim", lazy = true },
  { "catppuccin/nvim", name = "catppuccin", lazy = true },
  { "projekt0n/github-nvim-theme", lazy = true },
  { "marko-cerovac/material.nvim", lazy = true },
}
