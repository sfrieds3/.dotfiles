return {
  { "ramojus/mellifluous.nvim", opts = {
    transparent_background = {
      enabled = false,
    },
  } },
  { "loctvl842/monokai-pro.nvim", event = "VeryLazy" },
  {
    "projekt0n/github-nvim-theme",
    event = "VeryLazy",
    version = "*",

    config = function()
      require("github-theme").setup({
        options = {
          dim_inactive = false,
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
    event = "VeryLazy",
    name = "catppuccin",

    opts = {
      flavour = "mocha", -- latte, frappe, macchiato, mocha
      background = {
        light = "frappe",
        dark = "mocha",
      },
      term_colors = true,
      dim_inactive = {
        enabled = false,
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
      vim.g.gruvbox_material_dim_inactive_windows = 0
    end,
  },
  {
    "ellisonleao/gruvbox.nvim",
    event = "VeryLazy",
    opts = {
      invert_selection = false,
      invert_tabline = false,
      contrast = "hard",
    },
  },
  {
    "rebelot/kanagawa.nvim",
    event = "VeryLazy",

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
        dimInactive = false,
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
      })
    end,
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
    event = "VeryLazy",

    config = function()
      require("onedark").setup({
        style = "darker",
      })
    end,
  },
  {
    "sainnhe/sonokai",
    event = "VeryLazy",
    config = function()
      vim.g.sonokai_style = "shusia"
      vim.g.sonokai_dim_inactive_windows = false
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
  {
    "mcchrish/zenbones.nvim",
    event = "VeryLazy",
    dependencies = "rktjmp/lush.nvim",
  },
  {
    "rmehri01/onenord.nvim",
    event = "VeryLazy",
  },
  {
    "AlexvZyl/nordic.nvim",
    event = "VeryLazy",
  },
  {
    "kevinm6/kurayami.nvim",
    event = "VeryLazy",
  },
  {
    "comfysage/evergarden",
    event = "VeryLazy",
  },
  {
    "nyoom-engineering/oxocarbon.nvim",
    event = "VeryLazy",
  },
}
