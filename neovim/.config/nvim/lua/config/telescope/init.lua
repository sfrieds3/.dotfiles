local map = require('utils').mapper()
local telescope_config = require('config.telescope.telescope_config')

require('telescope').setup{
  defaults = {
    vimgrep_arguments = {
      'rg',
      '--color=never',
      '--no-heading',
      '--with-filename',
      '--line-number',
      '--column',
      '--smart-case',
      '--no-ignore',
      '--hidden'
    }
  }
}

local map_telescope = function(key, cmd, theme, theme_config, mode)
  theme_config = theme_config or "previewer = false"
  mode = mode or 'n'
  local base_command = "<cmd> lua require('telescope.builtin').%s(require('telescope.themes').get_%s({s}))<cr>"
  local command = string.format(base_command, cmd, theme, theme_config)
  map(mode, key, command)
end

map('n', '<Leader>ff', "<cmd>lua require('config.telescope.telescope_config').project_files()<CR>")
map('n', '<Leader>fh', "<cmd>lua require('telescope.builtin').help_tags()<cr>")

map_telescope('<Leader>fb', 'buffers', 'dropdown', "previewer = false")
map_telescope('<Leader>fr', 'oldfiles', 'ivy')
map_telescope('<Leader>f<Space>', 'oldfiles', 'ivy')
map_telescope('<Leader>tt', 'treesitter', 'ivy')
map_telescope('<Leader>tk', 'keymaps', 'dropdown')
map_telescope('<Leader>tk', 'keymaps', 'dropdown')
map_telescope('<Leader>gr', 'grep_string', 'dropdown')
map_telescope('<Leader>ts', 'search_history', 'dropdown', "previewer = false")
map_telescope('<Leader>tc', 'command_history', 'dropdown', "previewer = false")
map_telescope('<Leader>tj', 'jumplist', 'dropdown')
map_telescope('<Leader>tl', 'lsp_document_symbols', 'ivy')
map_telescope('<Leader>t<Space>', 'current_buffer_fuzzy_find', 'ivy')
map_telescope('<Leader>tm', 'marks', 'dropdown')
map_telescope('<Leader>tr', 'registers', 'dropdown')

map('n', '<Leader>tv', "<cmd>lua require('config.telescope.telescope_config').vim_options()<CR>")
map('n', '<Leader>tw', "<cmd>lua require('config.telescope.telescope_config').wiki_search()<CR>")
map('n', '<Leader>fg', "<cmd>lua require('config.telescope.telescope_config').live_grep()<CR>")

require('telescope').load_extension('fzy_native')

-- load local telescope config, if exists
pcall(require, 'config.telescope.local')
