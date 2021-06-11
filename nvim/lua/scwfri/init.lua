vim.cmd([[ 
  set runtimepath^=~/.vim
  set runtimepath+=~/.vim/after
  set runtimepath+=~/.config/nvim/colors
  set runtimepath+=~/.config/nvim/pack
  let &packpath = &runtimepath
]])

require('scwfri.config')
require('scwfri.pconfig')
require('scwfri.colors')
