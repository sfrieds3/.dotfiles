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

  -- testing
  use {
    'nvim-telescope/telescope.nvim',
    cmd = { 'Telescope' },
    keys = { '<leader>ff', '<leader>fg', '<leader>fb', '<leader>fh', },
    config = function()
      require('config.telescope')
    end,
    requires = {
      'nvim-lua/plenary.nvim',
      'nvim-lua/popup.nvim',
      'nvim-telescope/telescope-fzy-native.nvim',
      { 'nvim-treesitter/nvim-treesitter',
      config = function()
        require('config.treesitter')
      end
    }
  }
}

  -- quality of life
  use {
    { 'AckslD/nvim-neoclip.lua',
      requires = {
        { 'nvim-telescope/telescope.nvim' },
      },
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

  -- other utils
  use {
    { 'junegunn/fzf' },
    { 'junegunn/fzf.vim' },
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
    { 'hrsh7th/cmp-buffer' },
    { 'hrsh7th/cmp-cmdline' },
    { 'hrsh7th/cmp-nvim-lsp' },
    { 'hrsh7th/cmp-nvim-lsp-signature-help' },
    { 'hrsh7th/cmp-path' },
    { 'hrsh7th/cmp-vsnip' },
    { 'hrsh7th/nvim-cmp' },
    { 'hrsh7th/vim-vsnip' },
    { 'neovim/nvim-lspconfig' },
    { 'nvim-lua/lsp-status.nvim' },
    { 'quangnguyen30192/cmp-nvim-tags' },
    { 'rafamadriz/friendly-snippets' },
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
