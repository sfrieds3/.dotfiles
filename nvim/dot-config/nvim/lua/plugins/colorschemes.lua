local colorschemes = {
  {
    "webhooked/kanso.nvim",
    opts = {
      compile = true,
      background = {
        dark = "zen",
        light = "mist",
      },
      overrides = function(colors)
        return {
          WinSeparator = { fg = colors.theme.ui.nontext },
          StatusLine = { bg = colors.theme.ui.bg_p2 },
        }
      end,
    },
  },
  {
    "catppuccin/nvim",
    name = "catppuccin",

    opts = {
      flavour = "mocha",
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
    event = "VeryLazy",
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
    "bluz71/vim-moonfly-colors",
    name = "moonfly",
    event = "VeryLazy",
  },
  {
    "thesimonho/kanagawa-paper.nvim",
    event = "VeryLazy",
  },
  {
    "vague2k/vague.nvim",
    event = "VeryLazy",
  },
  {
    "rose-pine/neovim",
    name = "rose-pine",
    event = "VeryLazy",
  },
  {
    "projekt0n/github-nvim-theme",
    config = function(opts)
      require("github-theme").setup({
        options = {
          dim_inactive = true,
        },
        groups = {
          all = {
            StatusLine = { bg = "bg0" },
          },
        },
      })
    end,
  },
  {
    dir = vim.fn.stdpath("config") .. "/lua/calm",
    name = "calm",
    lazy = false,
    priority = 1000,
  },
}

return {}
