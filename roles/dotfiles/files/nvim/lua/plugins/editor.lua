return {
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
      { "<Leader>\\", "<Cmd>Neotree reveal_force_cwd<CR>" },
      { "<Leader>|", "<Cmd>Neotree reveal<CR>" },
      { "gd", "<Cmd>Neotree float reveal_file=<cfile> reveal_force_cwd<CR>" },
      { "<Leader>b", "<Cmd>Neotree toggle show buffers right<CR>" },
      { "<Leader>gs", "<Cmd>Neotree float git_status<CR>" },
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

    event = "VeryLazy",

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

    dependencies = {
      { "tami5/sqlite.lua", module = "sqlite" },
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
    "mbbill/undotree",

    cmd = "UndotreeToggle",

    keys = {
      {
        "_U",
        "<Cmd>exec('UndotreeToggle <bar> UndotreeFocus')<CR>",
        desc = "[U]ndo tree toggle & focus",
      },
      {
        "<Leader>u",
        "<Cmd>exec('UndotreeFocus')<CR>",
        desc = "[u]ndotree focus",
      },
    },

    config = function()
      vim.g.undotree_WindowLayout = 2

      vim.cmd([[
function! g:Undotree_CustomMap()
    nmap <buffer> K <plug>UndotreeNextState
    nmap <buffer> J <plug>UndotreePreviousState
    nmap <buffer> \u q
endfunction
]])
    end,
  },
  {
    "folke/todo-comments.nvim",

    dependencies = "nvim-lua/plenary.nvim",

    config = function()
      require("todo-comments").setup({})
      vim.keymap.set("n", "]T", function()
        require("todo-comments").jump_next()
      end, { desc = "Next todo comment" })

      vim.keymap.set("n", "[T", function()
        require("todo-comments").jump_prev()
      end, { desc = "Previous todo comment" })
      vim.keymap.set("n", "<Leader>vt", "<Cmd>TodoTelescope<CR>")
    end,
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
    "mrjones2014/smart-splits.nvim",

    config = function()
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

    opts = {
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
    },
  },
}
