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
        "<localleader>W",
        function()
          require("which-key").show({ global = true })
        end,
        desc = "All keymaps",
      },
      {
        "<localleader>w",
        function()
          require("which-key").show({ global = false })
        end,
        desc = "Buffer local keymaps",
      },
    },
  },
  {
    "nvim-neo-tree/neo-tree.nvim",
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
      "MunifTanjim/nui.nvim",
      {
        "s1n7ax/nvim-window-picker",
        version = "v2.*",
        opts = {
          hint = "floating-big-letter",
        },
        keys = {
          {
            "<Leader>w",
            function()
              local winid = require("window-picker"):pick_window() or vim.api.nvim_get_current_win()
              vim.api.nvim_set_current_win(winid)
            end,
            desc = "Nvim Window Picker: Pick a Window",
          },
        },
      },
    },
    opts = {
      buffers = {
        follow_current_file = {
          enabled = true,
        },
      },
      filesystem = {
        bind_to_cwd = false,
        follow_current_file = {
          enabled = true,
        },
        filtered_items = {
          hide_dotfiles = false,
          hide_gitignored = false,
          hide_hidden = false,
          hide_by_pattern = {
            "**/.git",
            "**/.DS_Store",
            "**/node_modules",
          },
        },
      },
      source_selector = {
        winbar = true,
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
  },
  {
    "stevearc/oil.nvim",
    cmd = "Oil",
    event = "VeryLazy",
    opts = {
      default_file_explorer = true,
      delete_to_trash = true,
      view_options = {
        show_hidden = true,
      },
    },
    -- stylua: ignore
    keys = {
      { "<leader>'", function() require("oil").toggle_float() end, desc = "Oil Toggle Float" },
      { "<leader>\"", "<cmd>Oil<cr>", desc = "Toggle Oil" },
    },
  },
  {
    "folke/trouble.nvim",
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
    },
    opts = {},
  },
  {
    "mrjones2014/smart-splits.nvim",

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
      -- TODO: convert these to lazy key
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
    "stevearc/quicker.nvim",
    opts = {
      keys = {
        {
          ">",
          function()
            require("quicker").expand({ before = 2, after = 2, add_to_existing = true })
          end,
          desc = "Expand quickfix context",
        },
        {
          "<",
          function()
            require("quicker").collapse()
          end,
          desc = "Collapse quickfix context",
        },
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
    opts = {
      highlight = {
        comments_only = false,
      },
      pattern = [[\b(KEYWORDS)\b]],
    },
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
      { "<leader>xt", "<cmd>TodoTrouble filter.buf=0<cr>", desc = "Todo (Trouble)" },
      { "<leader>xT", "<cmd>TodoTrouble keywords=TODO,FIX,FIXME<cr>", desc = "Todo/Fix/Fixme (Trouble)" },
      { "<leader>st", "<cmd>TodoFzfLua<cr>", desc = "Todo" },
      { "<leader>sT", "<cmd>TodoFzfLua keywords=TODO,FIX,FIXME<cr>", desc = "Todo/Fix/Fixme" },
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
      highlight_on_hover = false,
      layout = {
        min_width = { 40, 0.025 },
        max_width = { 80, 0.25 },
        default_direction = "prefer_right",
      },
      on_attach = function()
        require("aerial").tree_close_all()
      end,
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

    setup = function(opts)
      require("illuminate").configure(opts)
    end,

    keys = {
      {
        "[r",
        function()
          require("illuminate").next_reference({ reverse = true, wrap = true })
        end,
        desc = "Previous Reference",
      },
      {
        "]r",
        function()
          require("illuminate").next_reference({ wrap = true })
        end,
        desc = "Next Reference",
      },
    },
  },
  {
    "otavioschwanck/arrow.nvim",
    event = "VeryLazy",
    opts = {
      show_icons = true,
      leader_key = "<M-'>",
      buffer_leader_key = '<M-">',
    },
    -- stylua: ignore
    keys = {
      { "<leader>-", function() require("arrow.persist").previous() end, desc = "Arrow Previous", },
      { "<leader>=", function() require("arrow.persist").next() end, desc = "Arrow Next", },
      { "<C-s>", function() require("arrow.persist").toggle() end, desc = "Arrow Toggle", },
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
      { "<M-i>", "<cmd>IBLToggle<cr>", desc = "Toggle Indent Blankline" },
    },
  },
  {
    "gbprod/yanky.nvim",
    dependencies = {
      { "kkharji/sqlite.lua" },
    },
    opts = {
      ring = { storage = "sqlite" },
      highlight = {
        on_put = false,
        on_yank = false,
      },
    },
    keys = {
      { "<leader>p", "<cmd>YankyRingHistory<cr>", desc = "Open Yank History" },
      { "y", "<Plug>(YankyYank)", mode = { "n", "x" }, desc = "Yank text" },
      { "p", "<Plug>(YankyPutAfter)", mode = { "n", "x" }, desc = "Put yanked text after cursor" },
      { "P", "<Plug>(YankyPutBefore)", mode = { "n", "x" }, desc = "Put yanked text before cursor" },
      { "gp", "<Plug>(YankyGPutAfter)", mode = { "n", "x" }, desc = "Put yanked text after selection" },
      { "gP", "<Plug>(YankyGPutBefore)", mode = { "n", "x" }, desc = "Put yanked text before selection" },
      { "<c-p>", "<Plug>(YankyPreviousEntry)", desc = "Select previous entry through yank history" },
      { "<c-n>", "<Plug>(YankyNextEntry)", desc = "Select next entry through yank history" },
      { "]p", "<Plug>(YankyPutIndentAfterLinewise)", desc = "Put indented after cursor (linewise)" },
      { "[p", "<Plug>(YankyPutIndentBeforeLinewise)", desc = "Put indented before cursor (linewise)" },
      { "]P", "<Plug>(YankyPutIndentAfterLinewise)", desc = "Put indented after cursor (linewise)" },
      { "[P", "<Plug>(YankyPutIndentBeforeLinewise)", desc = "Put indented before cursor (linewise)" },
      { ">p", "<Plug>(YankyPutIndentAfterShiftRight)", desc = "Put and indent right" },
      { "<p", "<Plug>(YankyPutIndentAfterShiftLeft)", desc = "Put and indent left" },
      { ">P", "<Plug>(YankyPutIndentBeforeShiftRight)", desc = "Put before and indent right" },
      { "<P", "<Plug>(YankyPutIndentBeforeShiftLeft)", desc = "Put before and indent left" },
      { "=p", "<Plug>(YankyPutAfterFilter)", desc = "Put after applying a filter" },
      { "=P", "<Plug>(YankyPutBeforeFilter)", desc = "Put before applying a filter" },
    },
  },
}
