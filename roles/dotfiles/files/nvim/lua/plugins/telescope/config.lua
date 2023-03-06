local M = {}

function M.setup()
  local telescope = require("telescope")
  local builtin = require("telescope.builtin")
  local themes = require("telescope.themes")
  local custom = require("plugins.telescope.custom")
  local actions = require("telescope-live-grep-args.actions")
  local trouble = require("trouble.providers.telescope")

  telescope.setup({
    extensions = {
      ["fzf"] = {
        fuzzy = true,
        override_generic_sorter = true,
        override_file_sorter = true,
        case_mode = "smart_case",
      },
      ["ui-select"] = {
        themes.get_dropdown({}),
      },
      ["live_grep_args"] = {
        auto_quoting = true,
        mappings = {
          i = {
            ["<C-k>"] = actions.quote_prompt(),
            ["<C-i>"] = actions.quote_prompt({ postfix = " --iglob " }),
          },
        },
      },
    },
    defaults = {
      file_ignore_patterns = { "^tags$", "^TAGS$", "^.git/" },
      mappings = {
        i = { ["<C-l>"] = trouble.open_with_trouble },
        n = { ["<C-l>"] = trouble.open_with_trouble },
      },
    },
    pickers = {
      find_files = {
        -- find_command = { "rg", "--files", "--hidden", "-g", "!.git" },
        find_command = { "fd", "-t", "f", "--hidden", "--absolute-path" },
        layout_config = {
          height = 0.70,
        },
        theme = "ivy",
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
        theme = "dropdown",
      },
    },
  })

  pcall(telescope.load_extension, "frecency")
  pcall(telescope.load_extension, "fzf")
  pcall(telescope.load_extension, "neoclip")
  pcall(telescope.load_extension, "live_grep_args")
  pcall(telescope.load_extension, "aerial")
  pcall(telescope.load_extension, "ui-select")
  pcall(telescope.load_extension, "file_browser")
  pcall(telescope.load_extension, "dap")
end

return M
