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
    "projekt0n/github-nvim-theme",
    event = "VeryLazy",
    version = "*",

    config = function()
      require("github-theme").setup({
        options = {
          dim_inactive = true,
          styles = {
            functions = "italic",
          },
          inverse = { -- Inverse highlight for different types
            match_paren = true,
            visual = true,
            search = true,
          },
          darken = {
            floats = true,
            sidebars = {
              enable = true,
              list = { "qf", "terminal" },
            },
          },
        },
      })
    end,
  },
  {
    "catppuccin/nvim",
    name = "catppuccin",

    opts = {
      flavour = "mocha", -- latte, frappe, macchiato, mocha
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
      background = {
        dark = "dragon",
        light = "wave",
      },
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
    "marko-cerovac/material.nvim",
    event = "VeryLazy",

    config = function()
      vim.g.material_style = "darker"
      require("material").setup({
        contrast = {
          terminal = false, -- Enable contrast for the built-in terminal
          sidebars = true, -- Enable contrast for sidebar-like windows ( for example Nvim-Tree )
          floating_windows = false, -- Enable contrast for floating windows
          cursor_line = true, -- Enable darker background for the cursor line
          lsp_virtual_text = true,
          non_current_windows = true, -- Enable darker background for non-current windows
          filetypes = { "terminal", "lazy", "mason" }, -- Specify which filetypes get the contrasted (darker) background
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
          "flash",
          "gitsigns",
          "illuminate",
          "indent-blankline",
          "mini",
          "neo-tree",
          "neogit",
          "nvim-cmp",
          "telescope",
          "trouble",
          "which-key",
        },

        disable = {
          colored_cursor = true, -- Disable the colored cursor
          borders = false, -- Disable borders between verticaly split windows
          background = false, -- Prevent the theme from setting the background (NeoVim then uses your teminal background)
          term_colors = false, -- Prevent the theme from setting terminal colors
          eob_lines = false, -- Hide the end-of-buffer lines
        },

        high_visibility = {
          lighter = false, -- Enable higher contrast text for lighter style
          darker = true, -- Enable higher contrast text for darker style
        },

        async_loading = true, -- Load parts of the theme asyncronously for faster startup (turned on by default)

        custom_colors = nil, -- If you want to everride the default colors, set this to a function

        custom_highlights = {}, -- Overwrite highlights with your own
      })
    end,
  },
  {
    "sainnhe/sonokai",
    event = "VeryLazy",
    config = function()
      vim.g.sonokai_style = "shusia"
      vim.g.sonokai_dim_inactive_windows = true
      vim.g.sonokai_better_performance = true
    end,
  },
  {
    "folke/tokyonight.nvim",
    event = "VeryLazy",

    opts = {
      style = "night",
      dim_inactive = false,
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
  { "navarasu/onedark.nvim", event = "VeryLazy" },

  { "rose-pine/neovim", name = "rose-pine", event = "VeryLazy" },
  { "aliqyan-21/darkvoid.nvim", event = "VeryLazy" },
  { "nyoom-engineering/oxocarbon.nvim", event = "VeryLazy" },
}
