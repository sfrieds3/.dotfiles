vim.cmd([[ 
  "source ~/.vim/vimrc

  " neovim specific stuff
  set termguicolors
  augroup Neovim
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank { on_macro = true }
    augroup END

  " packadd cfilter
  packadd cfilter
]])

vim.g.mapleader = "\\"
vim.g.maplocalleader = "_"

vim.opt.showmode = false
vim.opt.inccommand = "split"
vim.opt.wildmode = "full"

vim.opt.hidden = true
vim.opt.autoread = true
vim.opt.modeline = false
vim.opt.ignorecase = true
vim.opt.infercase = true
vim.opt.smartcase = true
vim.opt.showmatch = true
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.autoindent = true
vim.opt.incsearch = true
vim.opt.hlsearch = true

local shiftwidth = 4
vim.opt.shiftwidth = shiftwidth
vim.opt.softtabstop = shiftwidth
vim.opt.tabstop = shiftwidth
vim.opt.scrolloff = 0
vim.opt.shiftround = true
vim.opt.expandtab = true
vim.opt.smarttab = true
vim.opt.timeout = false
vim.opt.ttimeout = true
vim.opt.ttimeoutlen = 10

vim.opt.laststatus = 2
vim.opt.backspace = { "indent", "eol", "start" }
vim.opt.encoding = "utf8"
vim.opt.fileencoding = "utf8"
vim.opt.showtabline = 3
vim.opt.clipboard:prepend({ "unnamed", "unnamedplus" })
vim.opt.foldmethod = "manual"
vim.opt.foldcolumn = "0"
vim.opt.mouse = "a"
vim.opt.formatoptions = "qrn1j"
vim.opt.nrformats:remove({ "octal" })
vim.opt.redrawtime = 50000
vim.opt.showbreak = "..."

vim.opt.listchars = { tab = "» ", extends = "›", precedes = "‹", nbsp = "␣", trail = "·" }
vim.opt.list = true

vim.cmd([[
  augroup ListChar
    autocmd!
    autocmd InsertEnter * set nolist
    autocmd InsertLeave * set list
  augroup END
]])

vim.opt.virtualedit = "block"

-- use rg for default grepprg
if vim.fn.executable("rg") then
  vim.opt_global.grepprg = "rg -HS --no-heading --vimgrep"
  vim.opt_global.errorformat = "%f:%l:%c:%m,%f"
else
  vim.opt_global.grepprg = "git grep -in $*"
end
