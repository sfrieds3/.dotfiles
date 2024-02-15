local M = {}

local open_with_trouble = function(...)
  return require("trouble.providers.telescope").open_with_trouble(...)
end
local open_selected_with_trouble = function(...)
  return require("trouble.providers.telescope").open_selected_with_trouble(...)
end

function M.setup()
  local telescope = require("telescope")
  local themes = require("telescope.themes")
  local custom = require("plugins.telescope.custom")
  local actions = require("telescope.actions")

  local fzf_opts = {
    fuzzy = true,
    override_generic_sorter = true,
    override_file_sorter = true,
    case_mode = "smart_case",
  }

  telescope.setup({
    extensions = {
      ast_grep = {
        command = {
          "sg",
          "--json=stream",
        }, -- must have --json=stream
        grep_open_files = false, -- search in opened files
        lang = nil, -- string value, specify language for ast-grep `nil` for default
      },
      fzf = fzf_opts,
      frecency = {
        auto_validate = false,
        db_safe_mode = false,
        show_scores = true,
        show_unindexed = true,
      },
      ["ui-select"] = {
        require("telescope.themes").get_dropdown({}),
      },
    },
    defaults = {
      file_ignore_patterns = { "^tags$", "^TAGS$", "^.git/", "^venv/" },
      mappings = {
        i = {
          ["<c-t>"] = open_with_trouble,
          ["<a-t>"] = open_selected_with_trouble,
          ["<C-Down>"] = actions.cycle_history_next,
          ["<C-Up>"] = actions.cycle_history_prev,
          ["<C-d>"] = actions.preview_scrolling_down,
          ["<C-u>"] = actions.preview_scrolling_up,
        },
        n = {
          ["<C-t>"] = open_with_trouble,
          ["l"] = actions.cycle_history_next,
          ["h"] = actions.cycle_history_prev,
          ["q"] = actions.close,
        },
      },
      buffer_preview_maker = custom.new_maker,
    },
    pickers = {
      find_files = {
        find_command = { "fd", "-t", "f", "--hidden", "--absolute-path", "--exclude", ".git", "--exclude", "venv" },
        layout_config = {
          height = 0.70,
        },
        follow = true,
        preview = {
          filesize_hook = function(filepath, bufnr, opts)
            local max_bytes = 10000
            local cmd = { "head", "-c", max_bytes, filepath }
            require("telescope.previewers.utils").job_maker(cmd, bufnr, opts)
          end,
        },
      },
      buffers = {
        show_all_buffers = true,
        sort_mru = true,
        mappings = {
          i = {
            ["<C-d>"] = "delete_buffer",
          },
        },
      },
      live_grep = {
        mappings = {
          i = { ["<c-f>"] = actions.to_fuzzy_refine },
        },
      },
      lsp_dynamic_workspace_symbols = {
        sorter = telescope.extensions.fzf.native_fzf_sorter(fzf_opts),
      },
    },
  })

  pcall(telescope.load_extension, "frecency")
  pcall(telescope.load_extension, "fzf")
  pcall(telescope.load_extension, "undo")
  pcall(telescope.load_extension, "harpoon")
  pcall(telescope.load_extension, "ui-select")
end

return M
