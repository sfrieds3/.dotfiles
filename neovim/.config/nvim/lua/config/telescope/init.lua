local map = require('utils').mapper()
local telescope_config = require('config.telescope.telescope_config')

require('telescope').setup{
  extensions = {
    fzf = {
      fuzzy = true,
      override_generic_sorter = true,
      override_file_sorter = true,
      case_mode = "smart_case",
    },
  },
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

map("n", "<Leader>ff", "<cmd>lua require('config.telescope.telescope_config').project_files()<CR>")
map("n", "<Leader>fo", "<cmd>lua require('config.telescope.telescope_config').old_files()<CR>")
map("n", "<Leader>fr", "<cmd>lua require('config.telescope.telescope_config').recent_files()<CR>")
map("n", "<Leader>ft", "<cmd>lua require('config.telescope.telescope_config').buffer_tags()<CR>")
map("n", "<Leader>fT", "<cmd>lua require('config.telescope.telescope_config').project_tags()<CR>")
map("n", "<Leader>fh", "<cmd>lua require('telescope.builtin').help_tags()<cr>")

map_telescope("<Leader>b", "buffers", "dropdown", "previewer = false")
map_telescope("<Leader>f<Space>", "oldfiles", "ivy")
map_telescope("<Leader>tt", "treesitter", "ivy")
map_telescope("<Leader>tk", "keymaps", "dropdown")
map_telescope("<Leader>tk", "keymaps", "dropdown")
map_telescope("<Leader>gr", "grep_string", "dropdown")
map_telescope("<Leader>ts", "search_history", "dropdown", "previewer = false")
map_telescope("<Leader>tc", "command_history", "dropdown", "previewer = false")
map_telescope("<Leader>tj", "jumplist", "dropdown")
map_telescope("<Leader>tl", "lsp_document_symbols", "ivy")
map_telescope("<Leader>t<Space>", "current_buffer_fuzzy_find", "ivy")
map_telescope("<Leader>tm", "marks", "dropdown")
map_telescope("<Leader>tr", "registers", "dropdown")
map_telescope("<Leader>tp", "neoclip", "dropdown")

map("n", "<Leader>tv", "<cmd>lua require('config.telescope.telescope_config').vim_options()<CR>")
map("n", "<Leader>tw", "<cmd>lua require('config.telescope.telescope_config').wiki_search()<CR>")
map("n", "<Leader>tc", "<cmd>lua require('config.telescope.telescope_config').edit_nvim_config()<CR>")
map("n", "<Leader>fg", "<cmd>lua require('config.telescope.telescope_config').live_grep()<CR>")
map("n", "<Leader>tgw", "<cmd>lua require('config.telescope.telescope_config').grep_wiki()<CR>")

require("telescope").load_extension("fzf")
require("telescope").load_extension("neoclip")

-- load local telescope config, if exists
pcall(require, "config.telescope.local")
