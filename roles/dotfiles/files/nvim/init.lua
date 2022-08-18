vim.cmd([[ runtime! config/pre*.vim ]])

require("utils")
local vim_config_path = string.format("%s/.vim", os.getenv("HOME"))
local data_dir = vim.fn.stdpath("data")
local config_path = vim.fn.stdpath("config")

vim.g.python3_host_prog = "$PYTHON3_VENV"

vim.opt.shell = "/usr/bin/zsh"
vim.opt.termguicolors = true
vim.opt.cursorline = true
vim.opt.hidden = true
vim.opt.autoread = true
vim.opt.modeline = false
vim.opt.ignorecase = true
vim.opt.infercase = true
vim.opt.smartcase = true
vim.opt.showmatch = true
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.incsearch = true
vim.opt.hlsearch = true
vim.opt.exrc = true
vim.opt.secure = true
vim.opt.showmode = false

--indentation
vim.opt.shiftround = true
vim.opt.expandtab = true
vim.opt.smarttab = true
vim.opt.tabstop = 8
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4
vim.opt.scrolloff = 3

vim.opt.laststatus = 3
-- vim.opt.winbar = "%=%m %F"
vim.opt.backspace = "indent,eol,start"
vim.opt.encoding = "utf8"
vim.opt.fileencoding = "utf8"
vim.opt.showtabline = 1
vim.opt.formatoptions = "qrn1j"
vim.opt.mouse = "a"
vim.opt.redrawtime = 50000
vim.opt.showbreak = "..."
vim.opt.foldenable = false
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
vim.opt.foldcolumn = "0"
vim.opt.signcolumn = "yes"

vim.opt.listchars = { tab = "» ", extends = "›", precedes = "‹", nbsp = "␣", trail = "·" }
vim.opt.list = true

vim.opt.timeout = true
vim.opt.timeoutlen = 500
vim.opt.ttimeout = true
vim.opt.ttimeoutlen = 10
vim.opt.updatetime = 500

vim.opt.wildmenu = true
vim.opt.wildignorecase = true
vim.opt.wildignore = "*.o,*.pyc,__pycache__/*,.venv/*,.*"
vim.opt.wildcharm = 26 -- <C-z>
vim.opt.tags = "./tags;,tags;"
vim.opt.completeopt = "menu,menuone,noselect"
vim.opt.path = ".,,"

vim.opt.undofile = true
vim.opt.backup = true
vim.opt.backupdir = vim.opt.backupdir - "."
vim.opt.backupext = ".bak"
vim.opt.swapfile = false
vim.opt.shada = [['1000,f1,<1000,/10000,:10000]]

vim.cmd([[ runtime! config/post*.vim ]])
