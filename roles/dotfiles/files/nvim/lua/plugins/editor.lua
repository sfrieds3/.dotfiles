return {
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    config = true,
    opts = {
      preset = "modern",
      spec = {
        {
          mode = { "n", "v" },
          { "g", group = "goto" },
          { "gs", group = "surround" },
          { "]", group = "next" },
          { "[", group = "prev" },
          { "<leader><tab>", group = "tabs" },
          { "<leader>b", group = "buffer" },
          { "<leader>c", group = "code" },
          { "<leader>f", group = "file/find" },
          { "<leader>g", group = "git/grep" },
          { "<leader>gd", group = "diff" },
          { "<leader>gh", group = "hunks" },
          { "<leader>gv", group = "diffview" },
          { "<leader>l", group = "lsp" },
          { "<leader>m", group = "marks" },
          { "<leader>r", group = "ripgrep" },
          { "<leader>s", group = "search" },
          { "<leader>u", group = "ui" },
          { "<leader>w", group = "windows" },
          { "<leader>x", group = "diagnostics/quickfix" },
        },
      },
    },
    keys = {
      {
        "__",
        function()
          require("which-key").show({ global = true })
        end,
        desc = "All keymaps",
      },
      {
        "_?",
        function()
          require("which-key").show({ global = false })
        end,
        desc = "Buffer local keymaps",
      },
    },
  },
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    cmd = "Neotree",
    keys = {
      { "<leader>\\", "<Cmd>Neotree reveal_force_cwd<CR>", desc = "Neotree Reveal CWD" },
      { "<leader>|", "<Cmd>Neotree reveal<CR>", desc = "Neotree Reveal" },
      {
        "<leader>s|",
        "<Cmd>Neotree float reveal_file=<cfile> reveal_force_cwd<CR>",
        desc = "Neotree Float Reveal CWD",
      },
      { "<leader>sb", "<Cmd>Neotree toggle show buffers right<CR>", desc = "Neotree Buffers" },
      { "<leader>gs", "<Cmd>Neotree float git_status<CR>", desc = "Neotee Git Status" },
    },
    dependencies = {
      "nvim-tree/nvim-web-devicons",
      "MunifTanjim/nui.nvim",
      {
        "s1n7ax/nvim-window-picker",
        version = "v2.*",
        config = true,
        keys = {
          {
            "<Leader>w",
            function()
              local winid = require("window-picker"):pick_window() or vim.api.nvim_get_current_win()
              vim.api.nvim_set_current_win(winid)
            end,
            desc = "Pick a window",
          },
        },
      },
    },
    config = function()
      vim.g.neo_tree_remove_legacy_commands = 1
      require("neo-tree").setup({
        opts = {
          filesystem = {
            bind_to_cwd = false,
            follow_current_file = true,
          },
          buffers = {
            follow_current_file = true,
          },
          window = {
            mappings = {
              ["<space>"] = "none",
            },
            fuzzy_finder_mappings = {
              ["<down>"] = "move_cursor_down",
              ["<C-n"] = "move_cursor_down",
              ["<up>"] = "move_cursor_up",
              ["<C-u"] = "move_cursor_up",
            },
          },
        },
      })
    end,
  },
  {
    "stevearc/oil.nvim",
    cmd = "Oil",
    opts = {
      delete_to_trash = true,
      show_hidden = true,
    },
    -- stylua: ignore
    keys = {
      { "<leader>'", function() require("oil").toggle_float() end, desc = "Oil Toggle Float" },
      { "<leader>\"", "<cmd>Oil<cr>", desc = "Toggle Oil" },
    },
  },
  {
    "folke/trouble.nvim",
    dependencies = "nvim-tree/nvim-web-devicons",
    cmd = "Trouble",

    keys = {
      { "<leader>xx", "<cmd>Trouble diagnostics toggle filter.buf=0<cr>", desc = "Buffer Diagnostics (Trouble)" },
      { "<leader>xX", "<cmd>Trouble diagnostics toggle<cr>", desc = "Diagnostics (Trouble)" },
      { "<leader>cs", "<cmd>Trouble symbols toggle focus=false<cr>", desc = "Symbols (Trouble)" },
      {
        "<leader>cl",
        "<cmd>Trouble lsp toggle focus=false win.position=right<cr>",
        desc = "LSP Definitions / references / ... (Trouble)",
      },
      { "<leader>xL", "<cmd>Trouble loclist toggle<cr>", desc = "Location List (Trouble)" },
      { "<leader>xQ", "<cmd>Trouble qflist toggle<cr>", desc = "Quickfix List (Trouble)" },
    },
    opts = {},
  },
  {
    "mrjones2014/smart-splits.nvim",
    -- build = "./kitty/install-kittens.bash",

    config = function()
      require("smart-splits").setup({
        -- Ignored filetypes (only while resizing)
        ignored_filetypes = {
          "nofile",
          "quickfix",
          "prompt",
          "Outline",
          "neo-tree",
        },
        -- Ignored buffer types (only while resizing)
        ignored_buftypes = { "neo-tree" },
        resize_mode = {
          -- key to exit persistent resize mode
          quit_key = "<ESC>",
          -- keys to use for moving in resize mode
          -- in order of left, down, up' right
          resize_keys = { "h", "j", "k", "l" },
          -- set to true to silence the notifications
          -- when entering/exiting persistent resize mode
          silent = false,
          -- must be functions, they will be executed when
          -- entering or exiting the resize mode
          hooks = {
            on_enter = nil,
            on_leave = nil,
          },
          multiplexer_integration = nil,
        },
      })
      -- TODO: conver these to lazy key
      vim.keymap.set("n", "<A-S-h>", require("smart-splits").resize_left, { desc = "smart-split resize left" })
      vim.keymap.set("n", "<A-S-j>", require("smart-splits").resize_down, { desc = "smart-split resize down" })
      vim.keymap.set("n", "<A-S-k>", require("smart-splits").resize_up, { desc = "smart-split resize up" })
      vim.keymap.set("n", "<A-S-l>", require("smart-splits").resize_right, { desc = "smart-split resize right" })
      -- moving between splits
      vim.keymap.set("n", "<A-h>", require("smart-splits").move_cursor_left, { desc = "smart-split move cursor left" })
      vim.keymap.set("n", "<A-j>", require("smart-splits").move_cursor_down, { desc = "smart-split move cursor down" })
      vim.keymap.set("n", "<A-k>", require("smart-splits").move_cursor_up, { desc = "smart-split move cursor up" })
      vim.keymap.set(
        "n",
        "<A-l>",
        require("smart-splits").move_cursor_right,
        { desc = "smart-split move cursor right" }
      )
      -- swapping buffers between windows
      vim.keymap.set("n", "<leader>Wh", require("smart-splits").swap_buf_left, { desc = "smart-split swap buf left" })
      vim.keymap.set("n", "<leader>Wj", require("smart-splits").swap_buf_down, { desc = "smart-split swap buf down" })
      vim.keymap.set("n", "<leader>Wk", require("smart-splits").swap_buf_up, { desc = "smart-split swap buf up" })
      vim.keymap.set("n", "<leader>Wl", require("smart-splits").swap_buf_right, { desc = "smart-split swap buf right" })
    end,
  },
  {
    "kevinhwang91/nvim-bqf",
    ft = "qf",
    dependencies = {
      {
        "junegunn/fzf",
        run = function()
          vim.fn["fzf#install"]()
        end,
      },
    },
    opts = {
      preview = {
        auto_preview = false,
      },
    },
  },
  {
    "folke/flash.nvim",

    opts = {
      search = {
        multi_window = false,
        trigger = "\\",
      },
    },

    -- stylua: ignore
    keys = {
      { "s", function() require("flash").jump() end, mode = { "n", "x", "o" }, desc = "Flash", },
      { "S", function() require("flash").treesitter() end, mode = { "n", "o", "x" }, desc = "Flash Treesitter", },
      { "r", function() require("flash").remote() end, mode = "o", desc = "Remote Flash", },
      { "R", function() require("flash").treesitter_search() end, mode = { "o", "x" }, desc = "Treesitter Search", },
      { "<c-s>", function() require("flash").toggle() end, mode = { "c" }, desc = "Toggle Flash Search", },
    },
  },
  {
    "folke/todo-comments.nvim",
    event = "BufRead",
    config = true,
    keys = {
      {
        "]T",
        function()
          require("todo-comments").jump_next()
        end,
        desc = "Next todo comment",
      },
      {
        "[T",
        function()
          require("todo-comments").jump_prev()
        end,
        desc = "Previous todo comment",
      },
      { "<leader>xt", "<cmd>TodoTrouble<cr>", desc = "Todo (Trouble)" },
      { "<leader>xT", "<cmd>TodoTrouble keywords=TODO,FIX,FIXME<cr>", desc = "Todo/Fix/Fixme (Trouble)" },
      { "<leader>st", "<cmd>TodoTelescope<cr>", desc = "Todo" },
      { "<leader>sT", "<cmd>TodoTelescope keywords=TODO,FIX,FIXME<cr>", desc = "Todo/Fix/Fixme" },
    },
  },
  {
    "dnlhc/glance.nvim",
    keys = {
      { "gD", "<CMD>Glance definitions<CR>", desc = "Glance: [g]lance [D]efinitions" },
      { "gR", "<CMD>Glance references<CR>", desc = "Glance: [g]lance [R]eferences" },
    },
    opts = {
      preview_win_opts = {
        winblend = 10,
      },
      border = {
        enable = true,
      },
      height = 33,
      theme = {
        mode = "auto",
      },
    },
  },
  {
    "Bekaboo/dropbar.nvim",
    event = "LspAttach",
    keys = {
      {
        "<leader>cn",
        function()
          require("dropbar.api").pick()
        end,
        desc = "Dropbar Pick",
      },
    },
    dependencies = {
      "nvim-telescope/telescope-fzf-native.nvim",
    },
    opts = {
      sources = {
        path = {
          modified = function(sym)
            return sym:merge({
              name = sym["name"] .. " [●]",
              name_hl = "StatuslineDiagnosticSignError",
              icon_hl = "StatuslineDiagnosticSignError",
            })
          end,
        },
      },
      icons = {
        ui = {
          bar = {
            separator = " ",
          },
        },
        menu = {
          separator = " ",
        },
      },
    },
  },
  {
    "andymass/vim-matchup",
    event = { "CursorMoved", "BufReadPost" },

    config = function()
      vim.g.matchup_matchparen_offscreen = { method = "popup" }
      vim.g.matchup_matchparen_deferred_show_delay = 500
      vim.g.matchup_matchparen_deferred_hide_delay = 500
      vim.g.matchup_matchparen_timeout = 100
      vim.g.matchup_matchparen_deferred = 1
    end,
  },
  {
    "stevearc/aerial.nvim",
    cmd = { "AerialOpen", "AerialToggle", "AerialNavOpen", "AerialNavToggle" },
    opts = {
      filter_kind = false,
      link_tree_to_folds = false,
      link_folds_to_tree = false,
      show_guides = true,
      layout = {
        min_width = { 40, 0.025 },
        max_width = { 80, 0.25 },
      },
    },
    -- Optional dependencies
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "nvim-tree/nvim-web-devicons",
    },
    keys = {
      { "<leader><cr>", "<cmd>AerialOpen<cr>", desc = "AerialOpen" },
    },
  },
  {
    "RRethy/vim-illuminate",

    opts = {
      providers = { "lsp", "treesitter", "regex" },
      delay = 200,
      large_file_cutff = 2000,
      large_file_override = {
        providers = { "lsp" },
      },
      filetypes_denylist = {
        "fugitive",
        "NvimTree",
        "TelescopePrompt",
      },
    },

    keys = {
      {
        "[[",
        function()
          require("illuminate").next_reference({ reverse = true, wrap = true })
        end,
        desc = "Previous Reference",
      },
      {
        "]]",
        function()
          require("illuminate").next_reference({ wrap = true })
        end,
        desc = "Next Reference",
      },
    },
  },
  {
    "monaqa/dial.nvim",

    -- stylua: ignore
    keys = {
      { "<C-a>", function() require("dial.map").manipulate("increment", "normal") end, desc = "Dial increment" },
      { "<C-x>", function() require("dial.map").manipulate("decrement", "normal") end, desc = "Dial decrement"},
      { "g<C-a>", function() require("dial.map").manipulate("increment", "gnormal") end, desc = "Dial increment" },
      { "g<C-x>", function() require("dial.map").manipulate("decrement", "gnormal") end, desc = "Dial decrement" },
      { "<C-a>", function() require("dial.map").manipulate("increment", "visual") end, mode = "v", desc = "Dial increment" },
      { "<C-x>", function() require("dial.map").manipulate("decrement", "visual") end, desc = "Dial decrement" },
      { "g<C-a>", function() require("dial.map").manipulate("increment", "gvisual") end, mode = "v", desc = "Dial increment" },
      { "g<C-x>", function() require("dial.map").manipulate("decrement", "gvisual") end, mode = "v", desc = "Dial decrement" },
    }
,
  },
  {
    "cbochs/grapple.nvim",
    name = "grapple",
    cmd = "Grapple",
    opts = {
      scope = "git_branch",
    },
    keys = {
      { "<leader>;", "<cmd>Grapple toggle_tags<cr>", desc = "Grapple Open Tags" },
      { "<leader>:", "<cmd>Telescope grapple tags<cr>", desc = "Grapple Open Tags Telescope" },
      { "<leader>=", "<cmd>Grapple tag<cr>", desc = "Grapple Tag" },
      { "<leader>-", "<cmd>Grapple untag<cr>", desc = "Grapple Untag" },
      { "<leader>]", "<cmd>Grapple cycle_forward<cr>", desc = "Grapple Cycle Forward" },
      { "<leader>[", "<cmd>Grapple cycle_backward<cr>", desc = "Grapple Cycle Backward" },
      { "<leader>1", "<cmd>Grapple select index=1<cr>", desc = "Grapple Select Index 1" },
      { "<leader>2", "<cmd>Grapple select index=2<cr>", desc = "Grapple Select Index 2" },
      { "<leader>3", "<cmd>Grapple select index=3<cr>", desc = "Grapple Select Index 3" },
      { "<leader>4", "<cmd>Grapple select index=4<cr>", desc = "Grapple Select Index 4" },
      { "<leader>5", "<cmd>Grapple select index=5<cr>", desc = "Grapple Select Index 5" },
      { "<leader>6", "<cmd>Grapple select index=6<cr>", desc = "Grapple Select Index 6" },
      { "<leader>7", "<cmd>Grapple select index=7<cr>", desc = "Grapple Select Index 7" },
      { "<leader>8", "<cmd>Grapple select index=8<cr>", desc = "Grapple Select Index 8" },
      { "<leader>9", "<cmd>Grapple select index=9<cr>", desc = "Grapple Select Index 9" },
    },
  },
  {
    "cbochs/portal.nvim",
    name = "portal",
    keys = {
      { "<leader>o", "<cmd>Portal jumplist backward<cr>", desc = "Portal Jumplist Backward" },
      { "<leader>i", "<cmd>Portal jumplist forward<cr>", desc = "Portal Jumplist Forward" },
    },
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    opts = {
      enabled = false,
      indent = {
        smart_indent_cap = true,
      },
      scope = {
        enabled = true,
      },
    },
    keys = {
      { "<leader>cl", "<cmd>IBLToggle<cr>", desc = "Toggle Indent Blankline" },
    },
  },
  {
    "mcauley-penney/visual-whitespace.nvim",
    config = true,
  },
}
