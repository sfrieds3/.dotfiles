vim.cmd[[set runtimepath^=~/.vim runtimepath+=~/.vim/after runtimepath+=~/.config/nvim/colors runtimepath+=~/.config/nvim/pack]]
vim.cmd[[let &packpath = &runtimepath]]
vim.cmd[[set shell=/usr/bin/zsh]]
vim.cmd[[source ~/.vim/vimrc]]

require('scwfri.colors')
require('scwfri.config')
require('scwfri.plugin')
