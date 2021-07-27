local utils = require('scwfri.utils')
local mapper = utils.mapper(nil)

require('telescope').setup {
  defaults = {
    vimgrep_arguments = {
      'rg',
      '--color=never',
      '--no-heading',
      '--with-filename',
      '--line-number',
      '--column',
      '--smart-case'
    },
    prompt_prefix = "> ",
    selection_caret = "> ",
    entry_prefix = "  ",
    initial_mode = "insert",
    selection_strategy = "reset",
    sorting_strategy = "descending",
    scroll_strategy = "cycle",
    layout_strategy = "horizontal",
    file_sorter =  require'telescope.sorters'.get_fuzzy_file,
    file_ignore_patterns = {},
    generic_sorter =  require'telescope.sorters'.get_generic_fuzzy_sorter,
    path_display = { "absolute" },
    winblend = 0,
    layout_config = {
      width = 0.75,
      prompt_position = "top",
      preview_cutoff = 120,
      horizontal = {
        mirror = false,
      },
      vertical = {
        mirror = false,
      },
    },
    border = {},
    borderchars = { '─', '│', '─', '│', '╭', '╮', '╯', '╰' },
    color_devicons = true,
    use_less = true,
    set_env = { ['COLORTERM'] = 'truecolor' }, -- default = nil,
    file_previewer = require'telescope.previewers'.vim_buffer_cat.new,
    grep_previewer = require'telescope.previewers'.vim_buffer_vimgrep.new,
    qflist_previewer = require'telescope.previewers'.vim_buffer_qflist.new,

    -- Developer configurations: Not meant for general override
    buffer_previewer_maker = require'telescope.previewers'.buffer_previewer_maker
  }
}

mapper('n', '\\ff', '<cmd>lua require("telescope.builtin").find_files()<CR>')
mapper('n', '\\ff', '<cmd>lua require("telescope.builtin").find_files()<CR>')
mapper('n', '\\fr', '<cmd>lua require("telescope.builtin").oldfiles()<CR>')
mapper('n', '\\ft', '<cmd>lua require("telescope.builtin").current_buffer_tags()<CR>')
mapper('n', '\\T', '<cmd>lua require("telescope.builtin").tags()<CR>')
mapper('n', '\\fb', '<cmd>lua require("telescope.builtin").buffers()<CR>')
mapper('n', '\\R', '<cmd>lua require("telescope.builtin").registers()<CR>')
mapper('n', '\\<Space>', '<cmd>lua require("telescope.builtin").current_buffer_fuzzy_find()<CR>')
