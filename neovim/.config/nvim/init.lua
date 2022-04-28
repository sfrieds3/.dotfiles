vim.cmd [[ runtime! config/pre*.vim ]]
vim.g.do_filetype_lua = 1
local g = vim.g
local cmd = vim.cmd
local o, wo, bo = vim.o, vim.wo, vim.bo
local vim_config_path = string.format('%s/.vim', os.getenv('HOME'))
local data_dir = vim.fn.stdpath('data')
local config_path = vim.fn.stdpath('config')

vim.g.python3_host_prog = '$PYTHON3_VENV'

require('plugins')
require('config')
local utils = require('utils')
local set = vim.opt

if vim.fn.executable('rg') then
  set.grepprg = 'rg -HS --no-heading --hidden --vimgrep'
else
  set.grepprg='git grep -in $*'
end

set.shell = '/usr/bin/zsh'
set.termguicolors = true
set.hidden = true
set.autoread = true
set.modeline = false
set.ignorecase = true
set.infercase = true
set.smartcase = true
set.showmatch = true
set.splitbelow = true
set.splitright = true
set.incsearch = true
set.hlsearch = true
set.exrc = true
set.secure = true
set.showmode = false

--indentation
set.shiftround = true
set.expandtab = true
set.smarttab = true
set.tabstop = 8
set.shiftwidth = 4
set.softtabstop = 4
set.scrolloff = 3

set.laststatus = 3
set.backspace = 'indent,eol,start'
set.encoding = 'utf8'
set.fileencoding = 'utf8'
set.showtabline = 3
set.formatoptions = 'qrn1j'
set.mouse = 'a'
set.redrawtime = 50000
set.showbreak = '...'
set.foldmethod = 'manual'
set.foldcolumn = '0'

set.listchars = { tab = '» ', extends = '›', precedes = '‹', nbsp = '␣', trail = '·' }
set.list = true

set.timeout = false
set.ttimeout = true
set.ttimeoutlen = 10

set.wildmenu = true
set.wildignorecase = true
set.wildignore = '*.o,*.pyc'
set.wildcharm = 26 -- <C-z>
set.tags = './tags;,tags;'
set.completeopt = 'menu,menuone,noselect'
set.path = '.,,'

set.undofile = true
set.backup = true
set.backupdir = vim.opt.backupdir - '.'
set.backupext = '.bak'
set.swapfile = false
set.shada = [['1000,f1,<1000,/10000,:10000]]

vim.cmd [[ runtime! config/post*.vim ]]
