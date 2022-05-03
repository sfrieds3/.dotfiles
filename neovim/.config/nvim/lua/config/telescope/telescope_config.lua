local M = {}
local themes = require('telescope.themes')

M.repo_files = function()
  require('telescope.builtin').git_files(themes.get_ivy())
end

M.project_files = function()
  require('telescope.builtin').find_files(themes.get_ivy({ hidden = true }))
end

function M.wiki_search()
  require('telescope.builtin').find_files {
    prompt_title = "~ wiki ~",
    path_display = { 'shorten' },
    cwd = "~/wiki/",

    layout_strategy = 'horizontal',
    layout_config = {
      preview_width = 0.35,
    },
  }
end

function M.edit_dotfiles()
  require('telescope.builtin').find_files {
    path_display = { 'shorten' },
    cwd = "~/.dotfiles/",
    prompt = "~ dotfiles ~",
    hidden = true,

    layout_strategy = 'horizontal',
    layout_config = {
      preview_width = 0.55,
    },
  }
end

function M.lsp_code_actions()
  local opts = themes.get_dropdown {
    winblend = 10,
    border = true,
    previewer = false,
    path_display = { 'shorten' },
  }

  require('telescope.builtin').lsp_code_actions(opts)
end

function M.live_grep()
  require('telescope.builtin').live_grep(themes.get_ivy {
    path_display = { 'shorten' },
    previewer = false,
    fzf_separator = "|>",
    hidden = true,
    layout_config = {
      prompt_position = 'bottom'
    }
  })
end

function M.grep_prompt()
  require('telescope.builtin').grep_string {
    path_display = { 'shorten' },
    search = vim.fn.input "Grep String > ",
  }
end

function M.grep_last_search(opts)
  opts = opts or {}

  -- \<getreg\>\C
  -- -> Subs out the search things
  local register = vim.fn.getreg("/"):gsub("\\<", ""):gsub("\\>", ""):gsub("\\C", "")

  opts.path_display = { 'shorten' }
  opts.word_match = "-w"
  opts.search = register

  require("telescope.builtin").grep_string(opts)
end

function M.search_all_files()
  require("telescope.builtin").find_files {
    find_command = { "rg", "--no-ignore", "--files" },
  }
end

function M.search_only_certain_files()
  require('telescope.builtin').find_files {
    find_command = {
      'rg',
      '--files',
      '--type',
      vim.fn.input "Type: ",
    },
  }
end

function M.lsp_references()
  require('telescope.builtin').lsp_references {
    layout_strategy = 'vertical',
    layout_config = {
      prompt_position = 'top',
    },
    sorting_strategy = 'ascending',
    ignore_filename = false,
  }
end

function M.lsp_implementations()
  require('telescope.builtin').lsp_implementations {
    layout_strategy = 'vertical',
    layout_config = {
      prompt_position = 'top',
    },
    sorting_strategy = 'ascending',
    ignore_filename = false,
  }
end

function M.vim_options()
  require('telescope.builtin').vim_options {
    layout_config = {
      width = 0.5,
    },
    sorting_strategy = 'ascending',
  }
end

function M.recent_files()
  require("telescope").extensions.frecency.frecency(themes.get_ivy { })
end

return M
