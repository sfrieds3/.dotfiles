local M = {}
local themes = require("telescope.themes")
local previewers = require("telescope.previewers")

function M.new_maker(filepath, bufnr, opts)
  opts = opts or {}

  filepath = vim.fn.expand(filepath)
  vim.loop.fs_stat(filepath, function(_, stat)
    if not stat then
      return
    end
    if stat.size > 100000 then
      return
    else
      previewers.buffer_previewer_maker(filepath, bufnr, opts)
    end
  end)
end

M.project_files = function(show_hidden)
  local hidden = show_hidden or false
  local opts = themes.get_ivy({
    hidden = hidden,
  })

  require("telescope.builtin").find_files(opts)
end

function M.wiki_search()
  local opts = {
    prompt_title = "~ wiki ~",
    path_display = { "shorten" },
    cwd = "~/wiki/",

    layout_strategy = "horizontal",
    layout_config = {
      preview_width = 0.35,
    },
  }

  require("telescope.builtin").find_files(opts)
end

function M.edit_dotfiles()
  local opts = {
    path_display = { "shorten" },
    cwd = "~/.dotfiles/",
    prompt = "~ dotfiles ~",
    hidden = true,
    no_ignore = true,

    layout_strategy = "horizontal",
    layout_config = {
      preview_width = 0.55,
    },
  }
  require("telescope.builtin").find_files(opts)
end

function M.edit_nvim_config()
  local opts = themes.get_ivy({
    path_display = { "shorten" },
    cwd = "~/.config/nvim/",
    prompt = "~ nvim ~",
    hidden = true,
    no_ignore = true,

    layout_config = {
      preview_width = 0.55,
    },
  })

  require("telescope.builtin").find_files(opts)
end

function M.lsp_code_actions()
  local opts = themes.get_dropdown({
    winblend = 10,
    border = true,
    previewer = false,
    path_display = { "shorten" },
  })

  require("telescope.builtin").lsp_code_actions(opts)
end

function M.grep_wiki()
  local opts = themes.get_dropdown({
    path_display = { "shorten" },
    cwd = "~/wiki/",
    prompt = "~ wiki live_grep ~",
    hidden = true,
  })

  require("telescope.builtin").live_grep(opts)
end

function M.live_grep()
  local opts = themes.get_ivy({
    previewer = false,
    fzf_separator = "|>",
    hidden = true,
    layout_config = {
      prompt_position = "bottom",
    },
  })

  require("telescope.builtin").live_grep(opts)
end

function M.live_grep_preview()
  local opts = themes.get_ivy({
    fzf_separator = "|>",
    hidden = true,
    layout_config = {
      prompt_position = "bottom",
    },
  })

  require("telescope.builtin").live_grep(opts)
end

function M.live_grep_args()
  local opts = themes.get_ivy({
    path_display = { "shorten" },
    hidden = true,
    previewer = false,
  })

  require("telescope").extensions.live_grep_args.live_grep_args(opts)
end

function M.grep_prompt()
  local opts = {
    path_display = { "shorten" },
    search = vim.fn.input("Grep String > "),
    previewer = false,
  }

  require("telescope.builtin").grep_string(opts)
end

function M.grep_last_search(opts)
  opts = opts or {}

  -- \<getreg\>\C
  -- -> Subs out the search things
  local register = vim.fn.getreg("/"):gsub("\\<", ""):gsub("\\>", ""):gsub("\\C", "")

  opts.path_display = { "shorten" }
  opts.word_match = "-w"
  opts.search = register
  opts.previewer = false

  require("telescope.builtin").grep_string(opts)
end

function M.grep_in_file()
  local opts = themes.get_ivy({})

  require("telescope.builtin").grep_string(opts)
end

function M.find_all_files()
  local opts = {
    find_command = {
      "fd",
      "-t",
      "f",
      "-H",
      "-I",
      "--absolute-path",
    },
  }

  require("telescope.builtin").find_files(opts)
end

function M.search_all_files()
  local opts = {
    find_command = {
      "rg",
      "--color=never",
      "--no-heading",
      "--with-filename",
      "--line-number",
      "--column",
      "--smart-case",
      "--trim",
      "--hidden",
      "-uuu",
    },
  }

  require("telescope.builtin").live_grep(opts)
end

function M.search_only_certain_files()
  local opts = {
    find_command = {
      "rg",
      "--files",
      "--type",
      vim.fn.input("Type: "),
    },
  }

  require("telescope.builtin").find_files(opts)
end

function M.lsp_references()
  local opts = {
    layout_strategy = "vertical",
    layout_config = {
      prompt_position = "top",
    },
    sorting_strategy = "ascending",
    ignore_filename = false,
  }

  require("telescope.builtin").lsp_references(opts)
end

function M.lsp_definitions()
  local opts = {
    layout_strategy = "vertical",
    layout_config = {
      prompt_position = "top",
    },
    sorting_strategy = "ascending",
    ignore_filename = false,
  }

  require("telescope.builtin").lsp_definitions(opts)
end

function M.lsp_incoming_calls()
  local opts = {
    layout_strategy = "vertical",
    layout_config = {
      prompt_position = "top",
    },
    sorting_strategy = "ascending",
    ignore_filename = false,
  }

  require("telescope.builtin").lsp_incoming_calls(opts)
end

function M.lsp_outgoing_calls()
  local opts = {
    layout_strategy = "vertical",
    layout_config = {
      prompt_position = "top",
    },
    sorting_strategy = "ascending",
    ignore_filename = false,
  }

  require("telescope.builtin").lsp_outgoing_calls(opts)
end

function M.lsp_implementations()
  local opts = {
    layout_strategy = "vertical",
    layout_config = {
      prompt_position = "top",
    },
    sorting_strategy = "ascending",
    ignore_filename = false,
  }

  require("telescope.builtin").lsp_implementations(opts)
end

function M.vim_options()
  local opts = {
    layout_config = {
      width = 0.5,
    },
    sorting_strategy = "ascending",
  }

  require("telescope.builtin").vim_options(opts)
end

function M.buffer_tags()
  local opts = require("telescope.themes").get_dropdown()

  require("telescope.builtin").current_buffer_tags(opts)
end

function M.project_tags()
  local opts = themes.get_dropdown()
  require("telescope.builtin").tags(opts)
end

function M.preview_lsp_references()
  local opts = themes.get_cursor({
    winblend = 10,
    layout_config = {
      width = 0.9,
    },
  })
  require("telescope.builtin").lsp_references(opts)
end

function M.preview_lsp_definitions()
  local opts = themes.get_cursor({
    winblend = 10,
    layout_config = {
      width = 0.9,
    },
  })
  require("telescope.builtin").lsp_definitions(opts)
end

function M.preview_lsp_incoming_calls()
  local opts = themes.get_cursor({
    winblend = 10,
    layout_config = {
      width = 0.75,
      height = 0.5,
    },
  })
  require("telescope.builtin").lsp_incoming_calls(opts)
end

function M.preview_lsp_outgoing_calls()
  local opts = themes.get_cursor({
    winblend = 10,
    layout_config = {
      width = 0.75,
      height = 0.5,
    },
  })
  require("telescope.builtin").lsp_outgoing_calls(opts)
end

function M.document_symbols()
  local opts = themes.get_dropdown({
    winblend = 10,
    layout_config = {
      width = 0.75,
    },
  })

  local active_clients = vim.lsp.get_clients({ bufnr = vim.fn.bufnr() })
  for _, client in ipairs(active_clients) do
    if client.server_capabilities.documentSymbolProvider ~= nil then
      require("telescope.builtin").lsp_document_symbols(opts)
      return
    end
  end
  require("telescope.builtin").treesitter(opts)
end

function M.workspace_symbols()
  local opts = themes.get_dropdown({
    winblend = 10,
    layout_config = {
      width = 0.75,
    },
  })

  local active_clients = vim.lsp.clients()
  for _, client in ipairs(active_clients) do
    if client.server_capabilities.workspaceSymbolProvider ~= nil then
      local bufnr = vim.fn.bufnr()
      local bufnrs = vim.lsp.get_buffers_by_client_id(client.id)
      for _, b in ipairs(bufnrs) do
        if b == bufnr then
          require("telescope.builtin").lsp_dynamic_workspace_symbols(opts)
          return
        end
      end

      -- TODO: run with arbitrary bufnr of client with workspace_symbol capabilities
      table.insert(opts, { bufnr = bufnrs[1] })
      require("telescope.builtin").lsp_dynamic_workspace_symbols(opts)
    end
  end
end

function M.installed_plugins()
  require("telescope.builtin").find_files({
    prompt = "~ installed plugin find file ~",
    cwd = vim.fn.stdpath("data") .. "/lazy/",
  })
end

function M.grep_installed_plugins()
  local opts = themes.get_ivy({
    path_display = { "shorten" },
    cwd = vim.fn.stdpath("data") .. "/lazy/",
    prompt = "~ installed plugin live_grep ~",
    hidden = true,
    layout_config = {
      prompt_position = "bottom",
      width = 0.75,
      preview_width = 0.55,
    },
  })

  require("telescope.builtin").live_grep(opts)
end

return M
