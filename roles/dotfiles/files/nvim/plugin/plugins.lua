local fn = vim.fn
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
  PACKER_BOOTSTRAP =
    fn.system({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path })
end

return require("packer").startup({
  function(use)
    use({ "wbthomason/packer.nvim" })

    -- local dev plugins
    local use_local = function(use_path)
      local local_path = vim.fn.expand("$HOME/dev/local_plugins")
      local repo = use_path
      for _, repo_name in string.gmatch(use_path, "(%w+)/([%w%W]+)") do
        repo = repo_name
      end

      local local_dir = string.format("%s/%s", local_path, repo)
      if vim.fn.isdirectory(local_dir) == 1 then
        use_path = local_dir
      end

      use({ use_path })
    end

    -- local plugins
    use_local("sfrieds3/pynvenv.nvim")

    -- git
    use({
      {
        "lewis6991/gitsigns.nvim",
        event = "BufReadPre",
        config = function()
          require("config.gitsigns")
        end,
      },
      { "tpope/vim-fugitive", cmd = "Git" },
      {
        "TimUntersberger/neogit",
        cmd = { "Neogit" },
        keys = { "_G" },
        requires = {
          "nvim-lua/plenary.nvim",
          "sindrets/diffview.nvim",
        },
        config = function()
          require("config.neogit")
        end,
      },
      { "rhysd/git-messenger.vim", cmd = { "GitMessenger" } },
      {
        "sindrets/diffview.nvim",
        requires = "nvim-lua/plenary.nvim",
        cmd = {
          "DiffviewOpen",
          "DiffviewClose",
          "DiffviewToggleFiles",
          "DiffviewFocusFiles",
          "DiffViewLog",
          "DiffViewFileHistory",
        },
        keys = { "_Dd", "_Dh", "_Dl" },
        config = function()
          require("config.diffview")
        end,
      },
    })

    -- nvim niceties
    use({
      {
        "windwp/nvim-autopairs",
        config = function()
          require("nvim-autopairs").setup({})
        end,
      },
      {
        "Shatur/neovim-session-manager",
        config = function()
          require("session_manager").setup({
            sessions_dir = vim.fn.expand(vim.fn.stdpath("data") .. "/sessions/"),
            autoload_mode = require("session_manager.config").AutoloadMode.Disabled,
            autosave_only_in_session = true,
          })
        end,
      },
      {
        "s1n7ax/nvim-window-picker",
        tag = "v1.*",
        config = function()
          require("window-picker").setup({})
          vim.keymap.set("n", "<Leader>w", function()
            local winid = require("window-picker").pick_window() or vim.api.nvim_get_current_win()
            vim.api.nvim_set_current_win(winid)
          end, { desc = "Pick a window" })
        end,
      },
      {
        "RRethy/vim-illuminate",
        config = function()
          require("config.illuminate")
        end,
      },
      {
        "akinsho/toggleterm.nvim",
        keys = { [[<C-\>]] },
        cmd = { "ToggleTerm" },
        config = function()
          require("config.toggleterm")
        end,
      },
      {
        "folke/todo-comments.nvim",
        requires = "nvim-lua/plenary.nvim",
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
        "nvim-lualine/lualine.nvim",
        config = function()
          require("config.lualine")
        end,
      },
      {
        "nvim-telescope/telescope.nvim",
        config = function()
          require("config.telescope")
          pcall(require, "config.telescope.local")
        end,
        requires = {
          "nvim-lua/plenary.nvim",
          "nvim-lua/popup.nvim",
          "nvim-telescope/telescope-live-grep-args.nvim",
          "nvim-telescope/telescope-ui-select.nvim",
          "nvim-telescope/telescope-project.nvim",
          "nvim-telescope/telescope-file-browser.nvim",
          { "nvim-telescope/telescope-frecency.nvim", requires = { "tami5/sqlite.lua" } },
          { "nvim-telescope/telescope-fzf-native.nvim", run = "make" },
        },
      },
      {
        "nvim-treesitter/nvim-treesitter",
        requires = {
          { "nvim-treesitter/playground", cmd = "TSPlaygroundToggle" },
          { "nvim-treesitter/nvim-treesitter-textobjects" },
          { "nvim-treesitter/nvim-treesitter-refactor" },
          { "nvim-treesitter/nvim-treesitter-context" },
        },
      },
      {
        "norcalli/nvim-colorizer.lua",
        ft = { "html", "css", "javascript", "vim", "eruby" },
        cmd = { "ColorizerToggle", "ColorizerAttachToBuffer" },
        config = function()
          require("config.colorizer")
        end,
      },
    })

    -- quality of life
    use({
      {
        "AckslD/nvim-neoclip.lua",
        requires = {
          { "nvim-telescope/telescope.nvim" },
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
      { "AndrewRadev/linediff.vim", cmd = { "LinediffAdd" } },
      { "chrisbra/NrrwRgn", cmd = { "NR", "NarrowRegion" } },
      { "numToStr/Navigator.nvim" },
      { "folke/lua-dev.nvim", ft = { "lua" } },
      {
        "rlane/pounce.nvim",
        keys = { "s", "S" },
        config = function()
          require("config.pounce")
        end,
      },
      { "RRethy/nvim-align", cmd = { "Align" } },
      {
        "nvim-tree/nvim-tree.lua",
        keys = "<Leader>\\",
        requires = {
          "kyazdani42/nvim-web-devicons",
        },
        config = function()
          require("nvim-tree").setup({})
          vim.keymap.set("n", "<Leader>\\", "<Cmd>NvimTreeToggle<CR>")
          vim.keymap.set("n", "_F", "<Cmd>NvimTreeFindFileToggle<CR>")
        end,
      },
      {
        "kevinhwang91/nvim-bqf",
        ft = { "qf" },
        config = function()
          require("config.bqf_config")
        end,
        requires = { "junegunn/fzf" },
      },
      {
        "ludovicchabant/vim-gutentags",
        config = function()
          vim.g.gutentags_cache_dir = vim.env.XDG_CACHE_HOME .. "/tags"
          if vim.fn.has("macunix") then
            vim.g.gutentags_ctags_executable = "/opt/homebrew/bin/ctags"
          end
        end,
      },
      { "stevearc/aerial.nvim" },
      { "mbbill/undotree", cmd = "UndotreeToggle" },
      { "romainl/vim-qf", ft = { "qf" } },
      { "romainl/vim-qlist" },
      {
        "tpope/vim-scriptease",
        cmd = {
          "Messages",
          "Verbose",
          "Time",
          "Scriptnames",
        },
      },
      { "tpope/vim-sleuth" },
      { "kylechui/nvim-surround" },
      {
        "numToStr/Comment.nvim",
        config = function()
          require("Comment").setup()
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
      { "wellle/targets.vim" },
      { "milisims/nvim-luaref", ft = { "lua" } },
    })

    -- debugging
    use({
      {
        "mfussenegger/nvim-dap",
        config = function()
          require("config.dap")
        end,
      },
      {
        "rcarriga/nvim-dap-ui",
        config = function()
          require("dapui").setup({})
          local dap, dapui = require("dap"), require("dapui")
          require("dap").listeners.after.event_initialized["dapui_config"] = function()
            require("dapui").open({})
          end
          require("dap").listeners.before.event_terminated["dapui_config"] = function()
            require("dapui").close({})
          end
          require("dap").listeners.before.event_exited["dapui_config"] = function()
            require("dapui").close({})
          end
        end,
        requires = { "mfussenegger/nvim-dap" },
      },
      { "nvim-telescope/telescope-dap.nvim" },
      {
        "mfussenegger/nvim-dap-python",
        config = function()
          require("dap-python").setup("~/.venv/venv/bin/python")
        end,
      },
      {
        "nvim-neotest/neotest",
        requires = {
          "nvim-lua/plenary.nvim",
          "nvim-treesitter/nvim-treesitter",
        },
        config = function()
          require("neotest").setup({
            adapters = {
              require("neotest-python")({
                justMyCode = false,
              }),
            },
          })
        end,
      },
      {
        "nvim-neotest/neotest-python",
      },
      { "ibhagwan/fzf-lua" },
    })

    -- lsp, completion
    use({
      {
        "dnlhc/glance.nvim",
        config = function()
          require("glance").setup({
            height = 33,
            theme = {
              mode = "brighten",
            },
          })
          vim.keymap.set("n", "gD", "<CMD>Glance definitions<CR>", { desc = "Glance: [g]lance [D]efinitions" })
          vim.keymap.set("n", "gR", "<CMD>Glance references<CR>", { desc = "Glance: [g]lance [R]eferences" })
        end,
      },
      {
        "folke/trouble.nvim",
        requires = "kyazdani42/nvim-web-devicons",
        cmd = { "Trouble", "TroubleToggle", "TroubleRefresh" },
        config = function()
          require("trouble").setup({})
        end,
      },
      {
        "hrsh7th/nvim-cmp",
        config = function()
          require("config.cmp")
        end,
        requires = {
          { "hrsh7th/cmp-buffer" },
          { "hrsh7th/cmp-cmdline" },
          { "hrsh7th/cmp-nvim-lua" },
          { "hrsh7th/cmp-path" },
          { "quangnguyen30192/cmp-nvim-tags" },
          { "saadparwaiz1/cmp_luasnip" },
        },
      },
      {
        "L3MON4D3/LuaSnip",
        config = function()
          require("luasnip.loaders.from_vscode").lazy_load()
          require("luasnip.loaders.from_lua").load({
            paths = os.getenv("XDG_CONFIG_HOME") .. "/nvim/.local_snippets.lua",
          })
        end,
        requires = {
          { "rafamadriz/friendly-snippets" },
        },
        after = "nvim-cmp",
        event = "InsertEnter",
      },
      {
        "neovim/nvim-lspconfig",
        config = function()
          require("config.lsp_config")
        end,
        requires = {
          { "nvim-lua/lsp-status.nvim" },
          { "hrsh7th/cmp-nvim-lsp" },
          { "hrsh7th/cmp-nvim-lsp-signature-help" },
        },
      },
      { "j-hui/fidget.nvim" },
      { "jose-elias-alvarez/null-ls.nvim" },
      { "jose-elias-alvarez/typescript.nvim" },
    })

    -- languages
    use({
      { "chrisbra/csv.vim", ft = "csv" },
      { "fatih/vim-go", ft = "go", disable = true },
      {
        "rust-lang/rust.vim",
        ft = "rust",
        config = function()
          vim.g.rustfmt_autosave = 1
        end,
      },
      { "tpope/vim-rails", ft = { "ruby", "eruby" } },
    })

    -- visuals
    use({
      {
        "NTBBloodbath/doom-one.nvim",
        setup = function()
          -- Add color to cursor
          vim.g.doom_one_cursor_coloring = false
          -- Set :terminal colors
          vim.g.doom_one_terminal_colors = true
          -- Enable italic comments
          vim.g.doom_one_italic_comments = true
          -- Enable TS support
          vim.g.doom_one_enable_treesitter = true
          -- Color whole diagnostic text or only underline
          vim.g.doom_one_diagnostics_text_color = false
          -- Enable transparent background
          vim.g.doom_one_transparent_background = false

          -- Pumblend transparency
          vim.g.doom_one_pumblend_enable = false
          vim.g.doom_one_pumblend_transparency = 20

          -- Plugins integration
          vim.g.doom_one_plugin_telescope = true
          vim.g.doom_one_plugin_neogit = true
          vim.g.doom_one_plugin_nvim_tree = true
          vim.g.doom_one_plugin_vim_illuminate = true
          vim.g.doom_one_plugin_neo_tree = true

          -- Pumblend transparency
          vim.g.doom_one_pumblend_enable = true
          vim.g.doom_one_pumblend_transparency = 20
        end,
      },
      { "sainnhe/edge", opt = true },
      { "sainnhe/gruvbox-material", opt = true },
      {
        "sainnhe/sonokai",
        opt = true,
        setup = function()
          vim.g.sonokai_style = "espresso"
          vim.g.sonokai_dim_inactive_windows = true
          vim.g.sonokai_better_performance = tru
        end,
      },
      { "navarasu/onedark.nvim", opt = true },
      { "catppuccin/nvim", as = "catppuccin", opt = true },
      { "projekt0n/github-nvim-theme" },
      { "marko-cerovac/material.nvim" },
    })

    -- Automatically set up your configuration after cloning packer.nvim
    -- Put this at the end after all plugins
    if PACKER_BOOTSTRAP then
      require("packer").sync()
    end
  end,
  config = { max_jobs = 25 },
})
