local M = {}
local themes = require('telescope.themes')

M.project_files = function(show_hidden)
  local hidden = show_hidden or false
  local opts = themes.get_ivy({
    hidden = hidden,
  })

  -- local ok = pcall(require('telescope.builtin').git_files, opts)
  require('telescope.builtin').find_files(opts)
end

function M.wiki_search()
  local opts = {
    prompt_title = '~ wiki ~',
    path_display = { 'shorten' },
    cwd = '~/wiki/',

    layout_strategy = 'horizontal',
    layout_config = {
      preview_width = 0.35,
    },
  }

  require('telescope.builtin').find_files(opts)
end

function M.edit_dotfiles()
  local opts = {
    path_display = { 'shorten' },
    cwd = '~/.dotfiles/',
    prompt = '~ dotfiles ~',
    hidden = true,
    no_ignore = true,

    layout_strategy = 'horizontal',
    layout_config = {
      preview_width = 0.55,
    },
  }
  require('telescope.builtin').find_files(opts)
end

function M.edit_nvim_config()
  local opts = themes.get_ivy({
    path_display = { 'shorten' },
    cwd = '~/.config/nvim/',
    prompt = '~ nvim ~',
    hidden = true,
    no_ignore = true,

    layout_config = {
      preview_width = 0.55,
    },
  })

  require('telescope.builtin').find_files(opts)
end

function M.lsp_code_actions()
  local opts = themes.get_dropdown({
    winblend = 10,
    border = true,
    previewer = false,
    path_display = { 'shorten' },
  })

  require('telescope.builtin').lsp_code_actions(opts)
end

function M.grep_wiki()
  local opts = themes.get_dropdown({
    path_display = { 'shorten' },
    cwd = '~/wiki/',
    prompt = '~ wiki live_grep ~',
    hidden = true,
  })

  require('telescope.builtin').live_grep(opts)
end

function M.live_grep()
  local opts = themes.get_ivy({
    path_display = { 'shorten' },
    previewer = false,
    fzf_separator = '|>',
    hidden = true,
    layout_config = {
      prompt_position = 'bottom'
    },
  })

  require('telescope.builtin').live_grep(opts)
end

function M.rg_raw_grep()
  local opts = themes.get_ivy({
    path_display = { 'shorten' },
    hidden = true,
    previewer = false,
  })

  require('telescope').extensions.live_grep_raw.live_grep_raw(opts)
end

function M.grep_prompt()
  local opts = {
    path_display = { 'shorten' },
    search = vim.fn.input 'Grep String > ',
  }

  require('telescope.builtin').grep_string(opts)
end

function M.grep_last_search(opts)
  opts = opts or {}

  -- \<getreg\>\C
  -- -> Subs out the search things
  local register = vim.fn.getreg('/'):gsub('\\<', ''):gsub('\\>', ''):gsub('\\C', '')

  opts.path_display = { 'shorten' }
  opts.word_match = '-w'
  opts.search = register

  require('telescope.builtin').grep_string(opts)
end

function M.grep_in_file()
  local opts = themes.get_ivy({})

  require('telescope.builtin').grep_string(opts)
end

function M.search_all_files()
  local opts = {
    find_command = {
      'rg',
      '--color=never',
      '--no-heading',
      '--with-filename',
      '--line-number',
      '--column',
      '--smart-case',
      '--trim'
    },
  }

  require('telescope.builtin').find_files(opts)
end

function M.search_only_certain_files()
  local opts = {
    find_command = {
      'rg',
      '--files',
      '--type',
      vim.fn.input 'Type: ',
    },
  }

  require('telescope.builtin').find_files(opts)
end

function M.lsp_references()
  local opts = {
    layout_strategy = 'vertical',
    layout_config = {
      prompt_position = 'top',
    },
    sorting_strategy = 'ascending',
    ignore_filename = false,
  }

  require('telescope.builtin').lsp_references(opts)
end

function M.lsp_implementations()
  local opts = {
    layout_strategy = 'vertical',
    layout_config = {
      prompt_position = 'top',
    },
    sorting_strategy = 'ascending',
    ignore_filename = false,
  }

  require('telescope.builtin').lsp_implementations(opts)
end

function M.vim_options()
  local opts = {
    layout_config = {
      width = 0.5,
    },
    sorting_strategy = 'ascending',
  }

  require('telescope.builtin').vim_options(opts)
end

function M.old_files()
  local opts = themes.get_ivy()

  require('telescope.builtin').oldfiles(opts)
end

function M.recent_files()
  local opts = themes.get_ivy()

  require('telescope').extensions.frecency.frecency(opts)
end

function M.buffer_tags()
  local opts = themes.get_dropdown()

  require('telescope.builtin').current_buffer_tags(opts)
end

function M.project_tags()
  local opts = themes.get_dropdown()
  require('telescope.builtin').tags(opts)
end

return M
