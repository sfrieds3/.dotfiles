return {
  {
    "dmtrKovalenko/fff.nvim",
    build = "cargo build --release",
    -- or if you are using nixos
    -- build = "nix run .#release",
    opts = {
      -- pass here all the options
    },
    keys = {
      {
        "<leader>ff",
        function()
          require("fff").find_files()
        end,
        desc = "Toggle FFF",
      },
      {
        "<localleader>F",
        function()
          require("fff").scan_files()
        end,
        desc = "FFF Rescan Files",
      },
    },
  },
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
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
    "stevearc/oil.nvim",
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
      {
        "<leader>xx",
        "<cmd>Trouble diagnostics toggle filter.buf=0 filter.severity=vim.diagnostic.severity.ERROR<cr>",
        desc = "Buffer Diagnostics (Trouble)",
      },
      {
        "<leader>xe",
        "<cmd>Trouble diagnostics filter.severity=vim.diagnostic.severity.ERROR<cr>",
        desc = "ERROR Level Diagnostics (Trouble)",
      },
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
    "folke/todo-comments.nvim",
    event = "BufRead",
    opts = {
      highlight = {
        comments_only = false,
      },
      pattern = [[\b(KEYWORDS)\b]],
    },
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
      { "<leader>xt", "<cmd>Trouble todo filter.buf=0<cr>", desc = "Todo (Trouble)" },
      {
        "<leader>xT",
        "<cmd>Trouble todo filter.tag=TODO,FIX,FIXME,XXX<cr>",
        desc = "Todo/Fix/Fixme (Trouble)",
      },
      { "<leader>st", "<cmd>TodoFzfLua<cr>", desc = "Todo" },
      { "<leader>sT", "<cmd>TodoFzfLua filter.tag=TODO,FIX,FIXME,XXX<cr>", desc = "Todo/Fix/Fixme" },
    },
  },
  {
    "mrjones2014/smart-splits.nvim",

    opts = {
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
    },
    init = function()
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
    opts = {
      sources = {
        path = {
          modified = function(sym)
            return sym:merge({
              name = sym["name"] .. " []",
              name_hl = "StatuslineDiagnosticSignError",
              icon_hl = "StatuslineDiagnosticSignError",
            })
          end,
        },
      },
    },
  },
  {
    "andymass/vim-matchup",
    event = { "CursorMoved", "BufReadPost" },

    init = function()
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
    "mistweaverco/kulala.nvim",
    keys = {
      { "<leader>Rs", desc = "Send request" },
      { "<leader>Ra", desc = "Send all requests" },
      { "<leader>Rb", desc = "Open scratchpad" },
    },
    ft = { "http", "rest" },
    opts = {
      global_keymaps = true,
    },
  },
}
