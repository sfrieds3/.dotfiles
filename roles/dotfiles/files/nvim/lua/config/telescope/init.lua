local map = require('utils').mapper()
local telescope_config = require('config.telescope.telescope_config')

require('telescope').setup({
  extensions = {
    -- fzf = {
    --   fuzzy = true,
    --   override_generic_sorter = true,
    --   override_file_sorter = true,
    --   case_mode = 'smart_case',
    -- },
    fzy = {
      override_generic_sorter = true,
      override_file_sorter = true,
    },
  },
  defaults = {
    file_ignore_patterns = { 'tags', 'TAGS', '.git' },
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
})

require('telescope').load_extension('fzy_native')
require('telescope').load_extension('neoclip')

local map_telescope = function(key, cmd, theme, theme_config, mode)
  theme_config = theme_config or 'previewer = false'
  mode = mode or 'n'
  local base_command = "<cmd> lua require('telescope.builtin').%s(require('telescope.themes').get_%s({s}))<cr>"
  local command = string.format(base_command, cmd, theme, theme_config)
  map(mode, key, command)
end

map('n', '<Leader>ff', "<cmd>lua require('config.telescope.telescope_config').project_files()<CR>")
map('n', '<Leader>fo', "<cmd>lua require('config.telescope.telescope_config').old_files()<CR>")
map('n', '<Leader>fr', "<cmd>lua require('config.telescope.telescope_config').recent_files()<CR>")
map('n', '<Leader>ft', "<cmd>lua require('config.telescope.telescope_config').buffer_tags()<CR>")
map('n', '<Leader>fT', "<cmd>lua require('config.telescope.telescope_config').project_tags()<CR>")
map('n', '<Leader>fh', "<cmd>lua require('telescope.builtin').help_tags()<cr>")

map_telescope('<Leader>fb', 'buffers', 'dropdown', 'previewer = false')
map_telescope('<Leader>f<Space>', 'oldfiles', 'ivy')
map_telescope('<Leader>tt', 'treesitter', 'ivy')
map_telescope('<Leader>tk', 'keymaps', 'dropdown')
map_telescope('<Leader>tk', 'keymaps', 'dropdown')
map_telescope('<Leader>gr', 'grep_string', 'dropdown')
map_telescope('<Leader>ts', 'search_history', 'dropdown', 'previewer = false')
map_telescope('<Leader>tc', 'command_history', 'dropdown', 'previewer = false')
map_telescope('<Leader>tj', 'jumplist', 'dropdown')
map_telescope('<Leader>tl', 'lsp_document_symbols', 'ivy')
map_telescope('<Leader>t<Space>', 'current_buffer_fuzzy_find', 'ivy')
map_telescope('<Leader>tm', 'marks', 'dropdown')
map_telescope('<Leader>tr', 'registers', 'dropdown')

map('n', '<Leader>tv', "<Cmd>lua require('config.telescope.telescope_config').vim_options()<CR>")
map('n', '<Leader>tw', "<Cmd>lua require('config.telescope.telescope_config').wiki_search()<CR>")
map('n', '<Leader>tc', "<Cmd>lua require('config.telescope.telescope_config').edit_nvim_config()<CR>")
map('n', '<Leader>fg', "<Cmd>lua require('config.telescope.telescope_config').live_grep()<CR>")
map('n', '<Leader>tgw', "<Cmd>lua require('config.telescope.telescope_config').grep_wiki()<CR>")
map('n', '<Leader>tgg', "<Cmd>lua require('config.telescope.telescope_config').rg_raw_grep()<CR>")
map('n', '<Leader>t/', "<Cmd>lua require('config.telescope.telescope_config').grep_last_search()<CR>")
map('n', '<Leader>tp', "<Cmd>lua require('telescope').extensions.neoclip.default()<CR>")

-- load local telescope config, if exists
pcall(require, 'config.telescope.local')
