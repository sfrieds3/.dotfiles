vim.cmd('set runtimepath^=~/.vim')
vim.cmd('set runtimepath+=~/.vim/after')
vim.cmd('let &packpath = &runtimepath')

require('scwfri.utils')
require('scwfri.config')
require('scwfri.pconfig')
require('scwfri.colors')
require('scwfri.keymap')
