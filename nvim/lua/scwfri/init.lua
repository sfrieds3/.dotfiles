vim.cmd('set runtimepath^=~/.vim')
vim.cmd('set runtimepath+=~/.vim/after')
vim.cmd('let &packpath = &runtimepath')

require('scwfri.config')
require('scwfri.pconfig')
require('scwfri.colors')
