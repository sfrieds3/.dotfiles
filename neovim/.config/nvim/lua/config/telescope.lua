local opts = { 'noremap' }
local map = require('utils').mapper(opts)
require('telescope').setup {
    extensions = {
        fzy_native = {
            override_generic_sorter = false,
            override_file_sorter = true,
        }
    }
}

map('n', '<leader>ff', "<cmd>lua require('telescope.builtin').find_files(require('telescope.themes').get_ivy({}))<cr>")
map('n', '<leader>fg', "<cmd>lua require('telescope.builtin').live_grep()<cr>")
map('n', '<leader>fb', "<cmd>lua require('telescope.builtin').buffers()<cr>")
map('n', '<leader>fh', "<cmd>lua require('telescope.builtin').help_tags()<cr>")

require('telescope').load_extension('fzy_native')
