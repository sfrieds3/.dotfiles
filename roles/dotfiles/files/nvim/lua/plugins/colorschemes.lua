return {
  { "sainnhe/edge" },
  { "nyoom-engineering/oxocarbon.nvim" },
  { "briones-gabriel/darcula-solid.nvim", dependencies = { "rktjmp/lush.nvim" } },
  { "nordtheme/vim", name = "nord" },
  { "loctvl842/monokai-pro.nvim" },
  { "Domeee/mosel.nvim" },
  { "polirritmico/monokai-nightasty.nvim" },
  {
    "catppuccin/nvim",
    name = "catppuccin",

    config = function()
      require("catppuccin").setup({
        flavour = "mocha", -- latte, frappe, macchiato, mocha
        background = {
          light = "frappe",
          dark = "mocha",
        },
        term_colors = true,
        dim_inactive = {
          enabled = true,
          shade = "dark",
          percentage = 0.15,
        },
        transparent_background = false,
        no_italic = false,
        no_bold = false,
        styles = {
          comments = { "italic" },
          conditionals = {},
          loops = {},
          functions = {},
          keywords = {},
          strings = {},
          variables = {},
          numbers = {},
          booleans = { "bold" },
          properties = {},
          types = {},
        },
        integrations = {
          cmp = true,
          dap = true,
          gitsigns = true,
          mason = true,
          navic = true,
          neogit = true,
          neotree = true,
          nvimtree = true,
          symbols_outline = true,
          telescope = true,
          treesitter = true,
          treesitter_context = true,
          lsp_trouble = true,
        },
        -- color_overrides = {
        --   mocha = {
        --     base = "#131313",
        --     mantle = "#313131",
        --     crust = "#333333",
        --   },
        -- },
      })
    end,
  },
  {
    "NTBBloodbath/doom-one.nvim",
    lazy = true,
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
  {

    "sainnhe/gruvbox-material",

    config = function()
      vim.g.gruvbox_material_background = "hard"
      vim.g.gruvbox_material_foreground = "material"
      vim.g.gruvbox_material_statusline_stype = "material"
      vim.g.gruvbox_better_performance = 1
      vim.g.gruvbox_material_dim_inactive_windows = 1
    end,
  },

  {
    "rebelot/kanagawa.nvim",

    config = function()
      vim.opt.fillchars:append({
        horiz = "━",
        horizup = "┻",
        horizdown = "┳",
        vert = "┃",
        vertleft = "┨",
        vertright = "┣",
        verthoriz = "╋",
      })
      require("kanagawa").setup({
        compile = true,
        dimInactive = true,
        globalStatus = true,
        terminalColors = true,
        background = {
          dark = "dragon",
          light = "wave",
        },
      })
    end,
  },
  {

    "marko-cerovac/material.nvim",

    config = function()
      vim.g.material_style = "darker"
      require("material").setup({
        contrast = {
          terminal = false, -- Enable contrast for the built-in terminal
          sidebars = true, -- Enable contrast for sidebar-like windows ( for example Nvim-Tree )
          floating_windows = false, -- Enable contrast for floating windows
          cursor_line = false, -- Enable darker background for the cursor line
          non_current_windows = true, -- Enable darker background for non-current windows
          filetypes = { "terminal", "packer" }, -- Specify which filetypes get the contrasted (darker) background
        },

        styles = { -- Give comments style such as bold, italic, underline etc.
          comments = { italic = true },
          strings = {},
          keywords = {},
          functions = { bold = true },
          variables = {},
          operators = {},
          types = {},
        },

        plugins = { -- Uncomment the plugins that you use to highlight them
          -- Available plugins:
          "dap",
          -- "dashboard",
          "gitsigns",
          -- "hop",
          "indent-blankline",
          -- "lspsaga",
          -- "mini",
          "neogit",
          "neorg",
          "nvim-cmp",
          "nvim-navic",
          -- "nvim-tree",
          -- "sneak",
          "telescope",
          "trouble",
          -- "which-key",
        },

        disable = {
          colored_cursor = false, -- Disable the colored cursor
          borders = false, -- Disable borders between verticaly split windows
          background = false, -- Prevent the theme from setting the background (NeoVim then uses your teminal background)
          term_colors = false, -- Prevent the theme from setting terminal colors
          eob_lines = false, -- Hide the end-of-buffer lines
        },

        high_visibility = {
          lighter = false, -- Enable higher contrast text for lighter style
          darker = true, -- Enable higher contrast text for darker style
        },

        lualine_style = "stealth", -- Lualine style ( can be 'stealth' or 'default' )

        async_loading = true, -- Load parts of the theme asyncronously for faster startup (turned on by default)

        custom_colors = nil, -- If you want to everride the default colors, set this to a function

        custom_highlights = {}, -- Overwrite highlights with your own
      })
    end,
  },
  {

    "navarasu/onedark.nvim",

    config = function()
      require("onedark").setup({
        style = "darker",
      })
    end,
  },
  {
    "sainnhe/sonokai",
    lazy = true,
    init = function()
      vim.g.sonokai_style = "shusia"
      vim.g.sonokai_dim_inactive_windows = true
      vim.g.sonokai_better_performance = true
    end,
  },
  {

    "folke/tokyonight.nvim",
    lazy = false,
    priority = 999,

    config = function()
      local tokyonight = require("tokyonight").setup({
        style = "night",
        dim_inactive = true,
        sidebars = {},
        styles = {
          comments = { italic = true },
          keywords = { italic = true },
          functions = {},
          variables = {},
          sidebars = "normal",
          floats = "transparent",
        },
      })

      -- require("tokyonight").load()
    end,
  },
}