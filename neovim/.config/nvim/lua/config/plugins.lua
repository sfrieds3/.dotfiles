local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  PACKER_BOOTSTRAP = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end

return require('packer').startup(function(use)
  use { 'wbthomason/packer.nvim' }

  -- git
  use {
    { 'airblade/vim-gitgutter',
       config = [[ vim.g.gitgutter_set_sign_backgrounds = 1
                   vim.g.gitgutter_use_location_list = 1 ]]
    },
    { 'tpope/vim-fugitive', cmd = 'Git' },
  }

  -- quality of life
  use {
    { 'AndrewRadev/linediff.vim', cmd = 'LineDiffAdd' },
    { 'chrisbra/NrrwRgn', cmd = { 'NR', 'NarrowRegion' } },
    { 'junegunn/vim-easy-align', keys = { 'gl' } },
    { 'justinmk/vim-dirvish' },
    { 'kevinhwang91/nvim-bqf' },
    { 'ludovicchabant/vim-gutentags' },
    { 'majutsushi/tagbar', cmd = 'TagbarToggle' },
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
    { 'hrsh7th/cmp-buffer' },
    { 'hrsh7th/cmp-cmdline' },
    { 'hrsh7th/cmp-nvim-lsp' },
    { 'hrsh7th/cmp-nvim-lsp-signature-help' },
    { 'hrsh7th/cmp-path' },
    { 'hrsh7th/cmp-vsnip' },
    { 'hrsh7th/nvim-cmp' },
    { 'hrsh7th/vim-vsnip' },
    { 'neovim/nvim-lspconfig' },
    { 'quangnguyen30192/cmp-nvim-tags' },
    { 'rafamadriz/friendly-snippets' },
  }

  -- languages
  use {
    { 'chrisbra/csv.vim', ft = 'csv' },
    { 'fatih/vim-go', ft = 'go' },
    { 'rust-lang/rust.vim', ft= 'rust', config = [[vim.g.rustfmt_autosave = 1]] },
    { 'mfulz/cscope.nvim', ft = { 'c', 'cop' } },
    { 'tpope/vim-rails', ft = { 'ruby', 'eruby' } },
  }

  -- visuals
  use {
    { 'sainnhe/edge' },
    { 'sainnhe/gruvbox-material' },
    { 'sainnhe/sonokai' },
    { 'vim-airline/vim-airline' },
  }

  -- Automatically set up your configuration after cloning packer.nvim
  -- Put this at the end after all plugins
  if PACKER_BOOTSTRAP then
    require('packer').sync()
  end
end)
