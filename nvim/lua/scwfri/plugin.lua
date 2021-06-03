require'lualine'.setup { 
  options = { 
    icons_enabled = false, 
    theme = 'auto', 
    component_separators = {'', ''}, 
    section_separators = {'', ''}, 
    disabled_filetypes = {} 
  }, 
  sections = { 
    lualine_a = {'mode'}, 
    lualine_b = {'branch'}, 
    lualine_c = {'filename'}, 
    lualine_x = {'encoding', 'fileformat', 'filetype'}, 
    lualine_y = {'progress'}, 
    lualine_z = {'location'} 
  }, 
  inactive_sections = { 
    lualine_a = {}, 
    lualine_b = {}, 
    lualine_c = {'filename'}, 
    lualine_x = {'location'}, 
    lualine_y = {}, 
    lualine_z = {} 
  }, 
  tabline = {}, 
  extensions = {} 
} 
vim.api.nvim_command('set noshowmode')

require('popup')

require('plenary')

require('telescope').setup{
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
    prompt_position = "bottom",
    prompt_prefix = "> ",
    selection_caret = "> ",
    entry_prefix = "  ",
    initial_mode = "insert",
    selection_strategy = "reset",
    sorting_strategy = "descending",
    scroll_strategy = "cycle",
    layout_strategy = "horizontal",
    layout_defaults = {
      horizontal = {
        mirror = false,
      },
      vertical = {
        mirror = false,
      },
    },
    file_sorter =  require'telescope.sorters'.get_fuzzy_file,
    file_ignore_patterns = {},
    generic_sorter =  require'telescope.sorters'.get_generic_fuzzy_sorter,
    shorten_path = true,
    winblend = 0,
    width = 0.75,
    preview_cutoff = 120,
    results_height = 1,
    results_width = 0.8,
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

require('bqf')
