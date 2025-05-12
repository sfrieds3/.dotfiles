return {
  {
    "ramojus/mellifluous.nvim",
    event = "VeryLazy",
    opts = {
      color_set = "mellifluous",
      transparent_background = {
        enabled = false,
      },
      dim_inactive = true,
    },
  },
  { "loctvl842/monokai-pro.nvim", event = "VeryLazy" },
  {
    "catppuccin/nvim",
    name = "catppuccin",

    opts = {
      flavour = "frappe",
      background = {
        light = "frappe",
        dark = "mocha",
      },
      dim_inactive = {
        enabled = true,
        shade = "dark",
        percentage = 0.80,
      },
      term_colors = true,
      styles = {
        booleans = { "bold" },
      },
      integrations = {
        aerial = true,
        blink_cmp = true,
        diffview = true,
        dropbar = {
          enabled = true,
          color_mode = true,
        },
        lsp_trouble = true,
        mason = true,
        overseer = true,
        which_key = true,
      },
      -- color_overrides = {
      --   mocha = {
      --     base = "#131313",
      --     mantle = "#313131",
      --     crust = "#333333",
      --   },
      -- },
    },
  },
  {
    "sainnhe/gruvbox-material",
    event = "VeryLazy",

    config = function()
      vim.g.gruvbox_material_background = "hard"
      vim.g.gruvbox_material_foreground = "material"
      vim.g.gruvbox_material_statusline_stype = "material"
      vim.g.gruvbox_better_performance = 1
      vim.g.gruvbox_material_dim_inactive_windows = 1
    end,
  },
  {
    "sainnhe/sonokai",
    event = "VeryLazy",
    config = function()
      vim.g.sonokai_style = "shusia"
      vim.g.sonokai_dim_inactive_windows = true
      vim.g.sonokai_better_performance = 1
    end,
  },
  {
    "sainnhe/edge",
    event = "VeryLazy",
    config = function()
      vim.g.edge_better_performance = 1
    end,
  },
  {
    "ellisonleao/gruvbox.nvim",
    event = "VeryLazy",
    opts = {
      invert_selection = false,
      invert_tabline = false,
      contrast = "hard",
      dim_inactive = true,
    },
  },
  {
    "rebelot/kanagawa.nvim",
    opts = {
      compile = true,
      dimInactive = true,
      globalStatus = true,
      terminalColors = true,
      colors = {
        theme = {
          all = {
            ui = {
              bg_gutter = "none",
            },
          },
        },
      },
    },
    config = true,
  },
  {
    "folke/tokyonight.nvim",
    event = "VeryLazy",

    opts = {
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
    },
  },
  {
    "WTFox/jellybeans.nvim",
    event = "VeryLazy",
    opts = {
      background = {
        dark = "jellybeans_muted", -- default dark palette
        light = "jellybeans_mono", -- default light palette
      },
    },
  },
  {
    "bluz71/vim-moonfly-colors",
    name = "moonfly",
  },
}
