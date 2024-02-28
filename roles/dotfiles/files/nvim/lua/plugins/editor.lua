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
        ["<leader>g"] = { name = "+git/grep" },
        ["<leader>gd"] = { name = "+diff" },
        ["<leader>gh"] = { name = "+hunks" },
        ["<leader>gv"] = { name = "+diffview" },
        ["<leader>h"] = { name = "+harpoon" },
        ["<leader>l"] = { name = "+lsp" },
        ["<leader>m"] = { name = "+marks" },
        ["<leader>q"] = { name = "+quit/session" },
        ["<leader>r"] = { name = "+ripgrep" },
        ["<leader>s"] = { name = "+search" },
        ["<leader>t"] = { name = "+tabs" },
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

    event = "WinEnter",

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
    cmd = { "Trouble", "TroubleToggle", "TroubleRefresh" },

    keys = {
      { "<leader>xx", "<cmd>TroubleToggle<cr>", desc = "Trouble Toggle" },
      { "<leader>xw", "<cmd>TroubleToggle workspace_diagnostics<cr>", desc = "Trouble Workspace Diagnostics" },
      { "<leader>xd", "<cmd>TroubleToggle document_diagnostics<cr>", desc = "Trouble Document Diagnostics" },
      { "<leader>xl", "<cmd>TroubleToggle loclist<cr>", desc = "Trouble Loclist" },
      { "<leader>xq", "<cmd>TroubleToggle quickfix<cr>", desc = "Trouble Quickfix" },
      { "<leader>xr", "<cmd>TroubleToggle lsp_references<cr>", desc = "Trouble LSP References" },
      { "<leader>xd", "<cmd>TroubleToggle lsp_definitions<cr>", desc = "Trouble LSP Definitions" },
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
  { "kylechui/nvim-surround", keys = { "cs", "ds", "ys" }, event = "InsertEnter", config = true },
  {
    "folke/flash.nvim",

    opts = {
      search = {
        multi_window = false,
        trigger = ";",
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
  {
    "luukvbaal/statuscol.nvim",
    opts = { setopt = true },
  },
  {
    "utilyre/barbecue.nvim",
    event = "LspAttach",
    name = "barbecue",
    version = "*",
    dependencies = {
      "SmiteshP/nvim-navic",
      "nvim-tree/nvim-web-devicons",
    },
    opts = {
      symbols = {
        separator = "",
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
    "hedyhli/outline.nvim",
    opts = {
      outline_items = {
        highlight_hovered_item = true,
        show_symbol_details = true,
        show_symbol_lineno = false,
      },
      outline_window = {
        show_cursorline = true,
        hide_cursor = false,
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
    "Wansmer/treesj",
    dependencies = "nvim-treesitter/nvim-treesitter",
    config = true,
    -- stylua: ignore
    keys = {
      { "<leader>cm", function() require("treesj").toggle() end, desc = "Treesj Toggle" },
    },
  },
  {
    "ThePrimeagen/harpoon",
    branch = "harpoon2",
    dependencies = { "nvim-lua/plenary.nvim" },

    config = function()
      local harpoon = require("harpoon")
      local extensions = require("harpoon.extensions")

      harpoon:setup()
      harpoon:extend(extensions.builtins.navigate_with_number())
      harpoon:extend({
        UI_CREATE = function(cx)
          vim.keymap.set("n", "<C-v>", function()
            harpoon.ui:select_menu_item({ vsplit = true })
          end, { buffer = cx.bufnr })

          vim.keymap.set("n", "<C-x>", function()
            harpoon.ui:select_menu_item({ split = true })
          end, { buffer = cx.bufnr })

          vim.keymap.set("n", "<C-t>", function()
            harpoon.ui:select_menu_item({ tabedit = true })
          end, { buffer = cx.bufnr })
        end,
      })
    end,

    -- stylua: ignore
    keys = {
      {"<leader>hh", function()
          local harpoon = require("harpoon")
          harpoon.ui:toggle_quick_menu(harpoon:list())
        end,
        desc = "Harpoon UI"
      },
      {"<leader>hH", function()
        local harpoon = require('harpoon')
          local conf = require("telescope.config").values
          local function toggle_telescope(harpoon_files)
            local file_paths = {}
            for _, item in ipairs(harpoon_files.items) do
              table.insert(file_paths, item.value)
            end

            require("telescope.pickers").new({}, {
              prompt_title = "Harpoon",
              finder = require("telescope.finders").new_table({
                results = file_paths,
              }),
              previewer = conf.file_previewer({}),
              sorter = conf.generic_sorter({}),
            }):find()
          end

          toggle_telescope(harpoon:list())
        end,
        desc = "Harpoon Telescope UI"
      },
      {"<leader>ha", function() require("harpoon"):list():append() end, desc = "Harpoon append"},
      { "<leader>hp", function() require("harpoon"):list():prev({ ui_nav_wrap = true }) end, desc = "Harpoon Previous"},
      { "<leader>hn", function() require("harpoon"):list():next({ ui_nav_wrap = true }) end, desc = "Harpoon Next"},

      { "<leader>h1", function() require("harpoon"):list():select(1) end, desc = "Harpoon Select Mark 1"},
      { "<leader>h2", function() require("harpoon"):list():select(2) end, desc = "Harpoon Select Mark 2"},
      { "<leader>h3", function() require("harpoon"):list():select(3) end, desc = "Harpoon Select Mark 3"},
      { "<leader>h4", function() require("harpoon"):list():select(4) end, desc = "Harpoon Select Mark 4"},
      { "<leader>h5", function() require("harpoon"):list():select(5) end, desc = "Harpoon Select Mark 5"},
      { "<leader>h6", function() require("harpoon"):list():select(6) end, desc = "Harpoon Select Mark 6"},
      { "<leader>h7", function() require("harpoon"):list():select(7) end, desc = "Harpoon Select Mark 7"},
      { "<leader>h8", function() require("harpoon"):list():select(8) end, desc = "Harpoon Select Mark 8"},
      { "<leader>h9", function() require("harpoon"):list():select(9) end, desc = "Harpoon Select Mark 9"},
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
}
