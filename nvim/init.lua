-- load configs
vim.cmd [[
if filereadable(glob('$HOME/.vim/autoload/pathogen.vim'))
    execute pathogen#infect('pack/bundle/start/{}')
    execute pathogen#infect('pack/bundle/opt/{}')
    execute pathogen#infect('colors/{}')
    execute pathogen#helptags()
endif
]]
require('scwfri')
