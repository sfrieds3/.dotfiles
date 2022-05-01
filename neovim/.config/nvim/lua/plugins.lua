local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  PACKER_BOOTSTRAP = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end

return require('packer').startup(function(use)
  use { 'wbthomason/packer.nvim' }

  -- git
  use {
    { 'lewis6991/gitsigns.nvim' },
    { 'tpope/vim-fugitive', cmd = 'Git' },
    { 'sindrets/diffview.nvim',
       requires = 'nvim-lua/plenary.nvim',
       cmd = { 'DiffviewOpen', 'DiffviewClose', 'DiffviewToggleFiles', 'DiffviewFocusFiles' }
    }
  }

  -- nvim niceties
  use {
    { 'nvim-telescope/telescope.nvim',
      -- cmd = { 'Telescope' },
      -- keys = { '<Leader>ff', '<Leader>fg', '<Leader>fb', '<Leader>fh', '<Leader>fr', '<leader>f<Space>', },
      config = function()
        require('config.telescope')
      end,
      requires = {
        'nvim-lua/plenary.nvim',
        'nvim-lua/popup.nvim',
        { 'nvim-telescope/telescope-frecency.nvim',
          config = function()
            require('telescope').load_extension('frecency')
          end,
          requires = { 'tami5/sqlite.lua' }
        },
        'nvim-telescope/telescope-fzy-native.nvim',
      }
    },
    { 'nvim-treesitter/nvim-treesitter',
      config = function()
        require('config.treesitter')
      end
    },
    { 'nvim-treesitter/playground' },
  }

  -- quality of life
  use {
    { 'AckslD/nvim-neoclip.lua',
      config = function()
        require('neoclip').setup()
      end,
    },
    { 'AndrewRadev/linediff.vim', cmd = 'LineDiffAdd' },
    { 'chrisbra/NrrwRgn', cmd = { 'NR', 'NarrowRegion' } },
    { 'ggandor/lightspeed.nvim',
      keys = { 'f', 'F', 't', 'T' },
      config = function()
        require('lightspeed').setup({
          repeat_ft_with_target_char = true,
          ignore_case = true,
        })
      end,
    },
    { 'junegunn/vim-easy-align', keys = { 'gl' } },
    { 'justinmk/vim-dirvish' },
    { 'kevinhwang91/nvim-bqf' },
    { 'ludovicchabant/vim-gutentags',
      config = function()
        vim.g.gutentags_cache_dir = vim.fn.stdpath('data') .. '/tags'
      end
    },
    { 'preservim/tagbar' },
    { 'mbbill/undotree', cmd = 'UndotreeToggle' },
    { 'phaazon/hop.nvim',
      keys = { 'gh' },
      cmd = { 'HopWord', 'HopChar1' },
      config = function()
        vim.api.nvim_set_keymap('n', 'gh', "<cmd> lua require'hop'.hint_words({ hint_position = require'hop.hint'.HintPosition.BEGIN })<cr>", {})
        vim.api.nvim_set_keymap('v', 'gh', "<cmd> lua require'hop'.hint_words({ hint_position = require'hop.hint'.HintPosition.BEGIN })<cr>", {})
        vim.api.nvim_set_keymap('o', 'gh', "<cmd> lua require'hop'.hint_words({ hint_position = require'hop.hint'.HintPosition.BEGIN, inclusive_jump = true })<cr>", {})
        require('hop').setup({})
      end,
    },
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
    { 'wellle/targets.vim' },
 }

  -- objects and stuf
  use {
    { 'tpope/vim-commentary' },
    { 'tpope/vim-repeat' },
    { 'tpope/vim-unimpaired' },
  }

  -- lsp, completion
  use {
    { 'folke/trouble.nvim',
      requires = 'kyazdani42/nvim-web-devicons',
      config = function()
        require('trouble').setup {
        }
      end
    },
    { 'hrsh7th/nvim-cmp',
      requires = {
        { 'hrsh7th/cmp-buffer' },
        { 'hrsh7th/cmp-cmdline' },
        { 'hrsh7th/cmp-nvim-lsp' },
        { 'hrsh7th/cmp-nvim-lsp-signature-help' },
        { 'hrsh7th/cmp-nvim-lua' },
        { 'hrsh7th/cmp-path' },
        { 'quangnguyen30192/cmp-nvim-tags' },
        { 'L3MON4D3/LuaSnip',
          config = function()
            require('luasnip.loaders.from_vscode').lazy_load()
          end,
          requires = {
            { 'rafamadriz/friendly-snippets' },
            { 'saadparwaiz1/cmp_luasnip' },
          },
        },
      },
    },
    { 'neovim/nvim-lspconfig',
      requires = { 'nvim-lua/lsp-status.nvim' },
    },
  }

  -- languages
  use {
    { 'chrisbra/csv.vim', ft = 'csv' },
    { 'fatih/vim-go', ft = 'go' },
    { 'rust-lang/rust.vim', ft= 'rust', config = [[vim.g.rustfmt_autosave = 1]] },
    { 'mfulz/cscope.nvim', ft = { 'c', 'cop' } },
    { 'psf/black', ft = { 'python' } },
    { 'tpope/vim-rails', ft = { 'ruby', 'eruby' } },
  }

  -- visuals
  use {
    { 'sainnhe/edge' },
    { 'sainnhe/gruvbox-material' },
    { 'sainnhe/sonokai' },
  }

  -- Automatically set up your configuration after cloning packer.nvim
  -- Put this at the end after all plugins
  if PACKER_BOOTSTRAP then
    require('packer').sync()
  end
end)
