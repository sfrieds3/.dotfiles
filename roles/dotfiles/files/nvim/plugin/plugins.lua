local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  PACKER_BOOTSTRAP = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end

return require('packer').startup(function(use)
  use { 'wbthomason/packer.nvim' }

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
      requires = {
        'nvim-lua/plenary.nvim',
        'sindrets/diffview.nvim',
      },
      config = function()
        require('neogit').setup {
          integrations = { diffview = true },
        }
      end,
    },
    { 'sindrets/diffview.nvim',
       requires = 'nvim-lua/plenary.nvim',
       cmd = { 'DiffviewOpen', 'DiffviewClose', 'DiffviewToggleFiles', 'DiffviewFocusFiles' }
    },
  }

  -- nvim niceties
  use {
    { 'akinsho/toggleterm.nvim' },
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
        { 'nvim-telescope/telescope-fzf-native.nvim', run = 'make', disable = true },
        { 'nvim-telescope/telescope-fzy-native.nvim' },
      }
    },
    { 'nvim-treesitter/nvim-treesitter',
      requires = {
        { 'nvim-treesitter/playground' },
        { 'nvim-treesitter/nvim-treesitter-textobjects' },
        { 'nvim-treesitter/nvim-treesitter-refactor' },
        { 'nvim-treesitter/nvim-treesitter-context' },
      },
    },
    { 'norcalli/nvim-colorizer.lua' },
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
    { 'AndrewRadev/linediff.vim', cmd = 'LineDiffAdd' },
    { 'chrisbra/NrrwRgn', cmd = { 'NR', 'NarrowRegion' } },
    { 'numToStr/Navigator.nvim' },
    { 'folke/lua-dev.nvim' },
    { 'ggandor/leap.nvim',
      keys = { 's', 'S' },
      config = function()
        require('leap').set_default_keymaps()
      end,
    },
    { 'RRethy/nvim-align' },
    { 'kyazdani42/nvim-tree.lua',
    },
    { 'kevinhwang91/nvim-bqf',
      ft = 'qf',
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
    { 'romainl/vim-qf' },
    { 'romainl/vim-qlist' },
    { 'tpope/vim-scriptease', cmd = {
        'Messages',
        'Verbose',
        'Time',
        'Scriptnames',
      },
    },
    { 'tpope/vim-surround' },
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
    { 'tpope/vim-repeat' },
    { 'wellle/targets.vim' },
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
  }

  -- languages
  use {
    { 'chrisbra/csv.vim', ft = 'csv' },
    { 'fatih/vim-go', ft = 'go' },
    { 'rust-lang/rust.vim', ft= 'rust', config = [[vim.g.rustfmt_autosave = 1]] },
    { 'mfulz/cscope.nvim', ft = { 'c', 'cpp' } },
    { 'psf/black', ft = { 'python' } },
    { 'tpope/vim-rails', ft = { 'ruby', 'eruby' } },
  }

  -- visuals
  use {
    { 'wincent/pinnacle' },
    { 'sainnhe/edge' },
    { 'sainnhe/gruvbox-material' },
    { 'sainnhe/sonokai' },
    { 'EdenEast/nightfox.nvim' },
    { 'folke/tokyonight.nvim' },
    { 'navarasu/onedark.nvim' },
    { 'arcticicestudio/nord-vim' },
    { 'catppuccin/nvim' },
    { 'marko-cerovac/material.nvim' },
    { 'RRethy/nvim-base16' },
    { 'projekt0n/github-nvim-theme' },
  }

  -- Automatically set up your configuration after cloning packer.nvim
  -- Put this at the end after all plugins
  if PACKER_BOOTSTRAP then
    require('packer').sync()
  end
end)
