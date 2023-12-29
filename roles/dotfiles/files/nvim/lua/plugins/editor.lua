return {
  {
    "folke/which-key.nvim",
    plugins = { spelling = true },
    opts = {
      defaults = {
        mode = { "n", "v" },
        ["g"] = { name = "+goto" },
        ["gs"] = { name = "+surround" },
        ["]"] = { name = "+next" },
        ["["] = { name = "+prev" },
        ["<leader><tab>"] = { name = "+tabs" },
        ["<leader>b"] = { name = "+buffer" },
        ["<leader>c"] = { name = "+code" },
        ["<leader>f"] = { name = "+file/find" },
        ["<leader>g"] = { name = "+git" },
        ["<leader>gh"] = { name = "+hunks" },
        ["<leader>gv"] = { name = "+diffview" },
        ["<leader>l"] = { name = "+lsp" },
        ["<leader>q"] = { name = "+quit/session" },
        ["<leader>r"] = { name = "+ripgrep" },
        ["<leader>s"] = { name = "+search" },
        ["<leader>u"] = { name = "+ui" },
        ["<leader>w"] = { name = "+windows" },
        ["<leader>x"] = { name = "+diagnostics/quickfix" },
      },
    },
    config = function(_, opts)
      local wk = require("which-key")
      wk.setup(opts)
      wk.register(opts.defaults)
    end,
  },
  {
    "levouh/tint.nvim",

    opts = {
      tint = -10,
      saturation = 0.6,
      window_ignore_function = function(winid)
        local exclude_filetypes = {
          ["neo-tree"] = true,
        }
        local bufid = vim.api.nvim_win_get_buf(winid)
        local buftype = vim.api.nvim_get_option_value("buftype", { buf = bufid })
        local floating = vim.api.nvim_win_get_config(winid).relative ~= ""
        local filetype = vim.api.nvim_get_option_value("filetype", { buf = bufid })

        -- Do not tint `terminal` or floating windows, tint everything else
        return buftype == "terminal" or floating or exclude_filetypes[filetype]
      end,
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
    "folke/trouble.nvim",
    dependencies = "nvim-tree/nvim-web-devicons",
    cmd = { "Trouble", "TroubleToggle", "TroubleRefresh" },

    keys = {
      { "<leader>xx", "<cmd>TroubleToggle<cr>", desc = "Trouble Toggle" },
      { "<leader>xw", "<cmd>TroubleToggle workspace_diagnostics<cr>", desc = "Trouble Workspace Diagnostics" },
      { "<leader>xd", "<cmd>TroubleToggle document_diagnostics<cr>", desc = "Trouble Document Diagnostics" },
      { "<leader>xl", "<cmd>TroubleToggle loclist<cr>", desc = "Trouble Loclist" },
      { "<leader>xq", "<cmd>TroubleToggle quickfix<cr>", desc = "Trouble Quickfix" },
    },
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
    ft = { "qf" },
    dependencies = { "junegunn/fzf" },
    opts = {
      auto_enable = true,
      magic_window = true,
      auto_resize_height = false,
      preview = {
        auto_preview = false,
      },
      filter = {
        fzf = {
          action_for = {
            ["ctrl-x"] = "split",
            ["ctrl-t"] = "tabedit",
            ["ctrl-v"] = "vsplit",
            ["ctrl-q"] = "signtoggle",
          },
          extra_opts = {
            description = "Extra options for fzf",
            default = { "--bind", "ctrl-o:toggle-all" },
          },
        },
      },
    },
  },
  { "kylechui/nvim-surround", config = true },
  {
    "folke/flash.nvim",

    opts = {
      search = {
        multi_window = false,
        trigger = ";",
      },
    },

    keys = {
      {
        "s",
        function()
          require("flash").jump()
        end,
        mode = { "n", "x", "o" },
        desc = "Flash",
      },
      {
        "S",
        function()
          require("flash").treesitter()
        end,
        mode = { "n", "o", "x" },
        desc = "Flash Treesitter",
      },
      {
        "r",
        function()
          require("flash").remote()
        end,
        mode = "o",
        desc = "Remote Flash",
      },
      {
        "R",
        function()
          require("flash").treesitter_search()
        end,
        mode = { "o", "x" },
        desc = "Treesitter Search",
      },
      {
        "<c-s>",
        function()
          require("flash").toggle()
        end,
        mode = { "c" },
        desc = "Toggle Flash Search",
      },
    },
  },
  {
    "AckslD/nvim-neoclip.lua",
    event = "VeryLazy",

    dependencies = {
      "tami5/sqlite.lua",
    },

    config = function()
      require("neoclip").setup({
        enable_macro_history = true,
        enable_persistent_history = true,
        continuous_sync = true,
      })
    end,
  },
  {
    "jedrzejboczar/possession.nvim",
    cmd = { "SLoad", "SSave", "SList" },
    opts = {
      autosave = {
        current = true,
        tmp = false,
      },
      plugins = {
        close_windows = {
          hooks = { "before_save", "before_load" },
          preserve_layout = true, -- or fun(win): boolean
          match = {
            floating = true,
            buftype = {},
            filetype = {},
            custom = false, -- or fun(win): boolean
          },
        },
        delete_hidden_buffers = false,
        nvim_tree = true,
        neo_tree = true,
        symbols_outline = true,
        tabby = true,
        dap = true,
        dapui = true,
        delete_buffers = false,
      },
      commands = {
        save = "SSave",
        load = "SLoad",
        delete = "SDelete",
        list = "SList",
        rename = "SRename",
        close = "SClose",
      },
    },

    keys = {
      {
        "\\S",
        function()
          require("telescope").extensions.possession.list()
        end,
        desc = "[S]essionManager: load session",
      },
    },
  },
  {
    "folke/todo-comments.nvim",
    cmd = { "TodoTrouble", "TodoTelescope" },
    config = true,
    keys = {
      {
        "]t",
        function()
          require("todo-comments").jump_next()
        end,
        desc = "Next todo comment",
      },
      {
        "[t",
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
  { "rcarriga/nvim-notify", event = "VeryLazy" },
  {
    "luukvbaal/statuscol.nvim",
    opts = { setopt = true },
  },
  {
    "utilyre/barbecue.nvim",
    name = "barbecue",
    version = "*",
    dependencies = {
      "SmiteshP/nvim-navic",
      "nvim-tree/nvim-web-devicons",
    },
    opts = {},
  },
  {
    "Wansmer/symbol-usage.nvim",
    enabled = false,
    event = "LspAttach",
    config = function()
      require("symbol-usage").setup({
        vt_position = "end_of_line",
      })
    end,
  },
  {
    "j-hui/fidget.nvim",
    dependencies = "neovim/nvim-lspconfig",
    event = "LspAttach",
    config = function()
      require("fidget").setup({
        progress = {
          suppress_on_insert = true,
          ignore_done_already = true,
          display = {
            render_limit = 3,
          },
        },
      })
    end,
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
  {
    "hedyhli/outline.nvim",
    opts = {
      outline_items = {
        highlight_hovered_item = true,
        show_symbol_details = true,
        show_symbol_lineno = false,
      },
      outline_window = {
        show_cursorline = true,
        hide_cursor = true,
      },
      symbol_folding = {
        autofold_depth = 1,
      },
      winblend = 10,
      keymaps = {
        hover_symbol = "<leader>e",
      },
    },
    cmd = { "Outline", "OutlineOpen" },
    keys = {
      { "<Leader><CR>", "<cmd>Outline<cr>", desc = "Outline: toggle" },
    },
  },
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
    "akinsho/bufferline.nvim",
    version = "*",
    opts = {
      options = {
        diagnostics = "nvim_lsp",
        mode = "tabs",
      },
    },
  },
  {
    "akinsho/toggleterm.nvim",
    keys = { [[<C-CR>]] },
    cmd = { "ToggleTerm" },

    config = function()
      require("toggleterm").setup({
        open_mapping = [[<C-\>]],
        hide_numbers = true,
        persist_size = true,
        persist_mode = true,
        shade_terminals = true,
      })

      vim.api.nvim_create_augroup("ToggleTerm", { clear = true })
      vim.api.nvim_create_autocmd({ "TermOpen" }, {
        group = "ToggleTerm",
        pattern = "term://*",
        callback = function()
          local buf_keymap_opts = { buffer = true }
          vim.keymap.set("t", "<esc>", [[<C-\><C-n>]], buf_keymap_opts)
          --vim.keymap.set('t', '<M-h>', [[<C-\><C-n><C-w>h]], buf_keymap_opts)
          --vim.keymap.set('t', '<M-j>', [[<C-\><C-n><C-w>j]], buf_keymap_opts)
          --vim.keymap.set('t', '<M-k>', [[<C-\><C-n><C-w>k]], buf_keymap_opts)
          --vim.keymap.set('t', '<M-l>', [[<C-\><C-n><C-w>l]], buf_keymap_opts)
        end,
      })
    end,
  },
}
