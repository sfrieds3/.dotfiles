-- load pathogen
local vim_config_path = string.format("%s/.vim", os.getenv('HOME'))
local config_path = vim.fn.stdpath("config")
local pathogen_path = string.format("%s/autoload/pathogen.vim", config_path)
-- TODO need to move this to a neovim-specific plugin folder
if vim.fn.filereadable(vim.fn.glob(pathogen_path)) then
    vim.fn['pathogen#infect']('pack/bundle/start/{}')
    vim.fn['pathogen#infect']('pack/bundle/opt/{}')
    vim.fn['pathogen#infect']('colors/{}')
    vim.fn['pathogen#helptags']()
end

-- load configs
require('scwfri')
