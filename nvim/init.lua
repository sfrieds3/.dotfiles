-- load pathogen
local config_path = vim.fn.stdpath("config")
local pathogen_path = string.format("%s/autoload/pathogen.vim", config_path)
if vim.fn.filereadable(vim.fn.glob(pathogen_path)) then
    vim.fn['pathogen#infect']('pack/bundle/start/{}')
    vim.fn['pathogen#infect']('pack/bundle/opt/{}')
    vim.fn['pathogen#infect']('colors/{}')
    vim.fn['pathogen#helptags']()
end

-- load configs
require('scwfri')
