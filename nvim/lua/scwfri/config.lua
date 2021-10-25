vim.cmd([[ 
  source ~/.vim/vimrc

  set shell=/usr/bin/zsh
  " neovim specific stuff
  "set termguicolors
  augroup Neovim
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank()
  augroup END

  packadd cfilter
  ]])

vim.opt.showmode = true
vim.o.inccommand = "split"
vim.o.wildmode = "full"
