local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  PACKER_BOOTSTRAP = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end

return require('packer').startup(function(use)
  use { 'wbthomason/packer.nvim', commit = 'de109156cfa634ce0256ea4b6a7c32f9186e2f10' }

  -- local dev plugins
  local use_local = function(use_path)
    local local_path = vim.fn.expand('$HOME/dev/plugins_nvim')
    local repo = use_path
    for _, repo_name in string.gmatch(use_path, '(%w+)/([%w%W]+)') do
      repo = repo_name
    end

    local local_dir = string.format('%s/%s', local_path, repo)
    if vim.fn.isdirectory(local_dir) == 1 then
      use_path = local_dir
    end

    use { use_path }
  end

  -- local plugins
  use_local('scwfri/pynvenv.nvim')

  -- git
  use {
    { 'lewis6991/gitsigns.nvim',
      event = 'BufReadPre',
      config = function()
        require('config.gitsigns')
      end
    },
    { 'tpope/vim-fugitive', cmd = 'Git' },
    { 'TimUntersberger/neogit',
      cmd = { 'Neogit' },
      keys = { '_G' },
      requires = {
        'nvim-lua/plenary.nvim',
        'sindrets/diffview.nvim',
      },
      config = function()
        require('config.neogit')
      end,
    },
    { 'sindrets/diffview.nvim',
       requires = 'nvim-lua/plenary.nvim',
       cmd = { 'DiffviewOpen', 'DiffviewClose', 'DiffviewToggleFiles', 'DiffviewFocusFiles', 'DiffViewLog', 'DiffViewFileHistory' },
       keys = { '_Dd', '_Dh', '_Dl' },
       config = function()
        require('config.diffview')
       end
    },
  }

  -- nvim niceties
  use {
    { 'RRethy/vim-illuminate',
        event = 'BufReadPre',
        config = function()
          vim.g.Illuminate_delay = 2500
        end,
    },
    { 'akinsho/toggleterm.nvim',
      keys = { [[<C-\>]] },
      cmd = { 'ToggleTerm' },
      config = function()
        require('config.toggleterm')
      end
    },
    { 'nvim-lualine/lualine.nvim',
      config = function()
       require('config.lualine')
      end
    },
    { 'nvim-telescope/telescope.nvim',
      config = function()
        require('config.telescope')
        pcall(require, 'config.telescope.local')
      end,
      requires = {
        'nvim-lua/plenary.nvim',
        'nvim-lua/popup.nvim',
        'nvim-telescope/telescope-live-grep-args.nvim',
        { 'nvim-telescope/telescope-frecency.nvim',
          requires = { 'tami5/sqlite.lua' }
        },
        { 'nvim-telescope/telescope-fzf-native.nvim', run = 'make' },
      }
    },
    { 'nvim-treesitter/nvim-treesitter',
      requires = {
        { 'nvim-treesitter/playground', cmd = 'TSPlaygroundToggle' },
        { 'nvim-treesitter/nvim-treesitter-textobjects' },
        { 'nvim-treesitter/nvim-treesitter-refactor' },
        { 'nvim-treesitter/nvim-treesitter-context' },
      },
    },
    { 'norcalli/nvim-colorizer.lua',
      ft = { 'html', 'css', 'javascript', 'vim', 'eruby' },
      cmd = { 'ColorizerToggle', 'ColorizerAttachToBuffer' },
      config = function()
        require('config.colorizer')
      end
    },
  }

  -- quality of life
  use {
    { 'AckslD/nvim-neoclip.lua',
      requires = {
        { 'nvim-telescope/telescope.nvim' },
        { 'tami5/sqlite.lua', module = 'sqlite'}
      },
      config = function()
        require('neoclip').setup {
          enable_macro_history = true,
          enable_persistent_history = true,
          continuous_sync = true,
        }
      end,
    },
    { 'AndrewRadev/linediff.vim', cmd = { 'LineDiffAdd' }, },
    { 'chrisbra/NrrwRgn', cmd = { 'NR', 'NarrowRegion' }, },
    { 'numToStr/Navigator.nvim' },
    { 'folke/lua-dev.nvim', ft = { 'lua' }, },
    { 'ggandor/leap.nvim',
      keys = { 's', 'S' },
      config = function()
        require('leap').set_default_keymaps()
      end,
    },
    { 'RRethy/nvim-align' },
    { 'nvim-neo-tree/neo-tree.nvim',
      cmd = { 'Neotree', 'NeoTreeRevealToggle' },
      keys = '<Leader><Space>',
      config = function()
        require('config.neotree')
      end,
      branch = 'v2.x',
      requires = {
        'nvim-lua/plenary.nvim',
        'kyazdani42/nvim-web-devicons',
        'MunifTanjim/nui.nvim',
        "MunifTanjim/nui.nvim",
      {
        's1n7ax/nvim-window-picker',
        tag = "v1.*",
        config = function()
          require'window-picker'.setup({
            autoselect_one = true,
            include_current = false,
            filter_rules = {
              -- filter using buffer options
              bo = {
                -- if the file type is one of following, the window will be ignored
                filetype = { 'neo-tree', "neo-tree-popup", "notify", "quickfix" },

                -- if the buffer type is one of following, the window will be ignored
                buftype = { 'terminal' },
              },
            },
            other_win_hl_color = '#e35e4f',
          })
        end,
      }
      },
    },
    { 'kevinhwang91/nvim-bqf',
      ft = { 'qf' },
      config = function()
        require('config.bqf_config')
      end
    },
    { 'ludovicchabant/vim-gutentags',
      config = function()
        vim.g.gutentags_cache_dir = vim.fn.stdpath('data') .. '/tags'
        --vim.g.gutentags_modules = { 'ctags', 'cscope', 'pycscope' }
      end
    },
    { 'stevearc/aerial.nvim' },
    { 'dstein64/vim-startuptime', cmd = 'StartupTime', },
    { 'mbbill/undotree', cmd = 'UndotreeToggle' },
    { 'romainl/vim-qf', ft = { 'qf' }, },
    { 'romainl/vim-qlist' },
    { 'tpope/vim-scriptease', cmd = {
        'Messages',
        'Verbose',
        'Time',
        'Scriptnames',
      },
    },
    { 'kylechui/nvim-surround' },
    { 'tversteeg/registers.nvim', keys = { { 'n', '"' }, { 'i', '<c-r>' } } },
    { 'numToStr/Comment.nvim',
    config = function()
        require('Comment').setup()
    end
    },
    { 'andymass/vim-matchup',
      config = function()
        vim.g.matchup_matchparen_offscreen = { method = 'popup' }
        vim.g.matchup_matchparen_deferred_show_delay = 500
        vim.g.matchup_matchparen_deferred_hide_delay = 500
        vim.g.matchup_matchparen_timeout = 100
        vim.g.matchup_matchparen_deferred = 1
      end
    },
    { 'wellle/targets.vim' },
    { 'milisims/nvim-luaref' },
  }

  -- lsp, completion
  use {
    { 'folke/trouble.nvim',
      requires = 'kyazdani42/nvim-web-devicons',
      cmd = { 'Trouble', 'TroublToggle', 'TroubleRefresh', },
      config = function()
        require('trouble').setup {
        }
      end,
    },
    { 'hrsh7th/nvim-cmp',
      config = function()
        require('config.cmp')
      end,
      requires = {
        { 'hrsh7th/cmp-buffer' },
        { 'hrsh7th/cmp-cmdline' },
        { 'hrsh7th/cmp-nvim-lua' },
        { 'hrsh7th/cmp-path' },
        { 'quangnguyen30192/cmp-nvim-tags' },
        { 'saadparwaiz1/cmp_luasnip' },
      },
    },
    { 'L3MON4D3/LuaSnip',
      config = function()
        require('luasnip.loaders.from_vscode').lazy_load()
      end,
      requires = {
        { 'rafamadriz/friendly-snippets' },
      },
      after = 'nvim-cmp',
      event = 'InsertEnter',
    },
    { 'neovim/nvim-lspconfig',
      config = function()
        require('config.lsp_config')
      end,
      requires = {
        { 'nvim-lua/lsp-status.nvim' },
        { 'hrsh7th/cmp-nvim-lsp' },
        { 'hrsh7th/cmp-nvim-lsp-signature-help' },
      },
    },
    { 'j-hui/fidget.nvim' },
  }

  -- languages
  use {
    { 'chrisbra/csv.vim', ft = 'csv' },
    { 'fatih/vim-go', ft = 'go' },
    { 'rust-lang/rust.vim', ft= 'rust',
      config = function()
        vim.g.rustfmt_autosave = 1
      end
    },
    { 'mfulz/cscope.nvim', ft = { 'c', 'cpp' } },
    { 'tpope/vim-rails', ft = { 'ruby', 'eruby' } },
  }

  -- visuals
  use {
    { 'NTBBloodbath/doom-one.nvim' },
    { 'sainnhe/edge', opt = true },
    { 'sainnhe/gruvbox-material', opt = true },
    { 'sainnhe/sonokai', opt = true },
    { 'EdenEast/nightfox.nvim', opt = true },
    { 'navarasu/onedark.nvim', opt = true },
    { 'catppuccin/nvim', as = 'catppuccin', opt = true },
    { 'projekt0n/github-nvim-theme' },
  }

  -- Automatically set up your configuration after cloning packer.nvim
  -- Put this at the end after all plugins
  if PACKER_BOOTSTRAP then
    require('packer').sync()
  end
end)
