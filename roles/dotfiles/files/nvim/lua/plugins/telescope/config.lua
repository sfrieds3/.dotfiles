local M = {}

function M.setup()
  local telescope = require("telescope")
  local builtin = require("telescope.builtin")
  local themes = require("telescope.themes")
  local custom = require("plugins.telescope.custom")
  local lga_actions = require("telescope-live-grep-args.actions")
  local actions = require("telescope.actions")
  local trouble = require("trouble.providers.telescope")

  local fzf_opts = {
    fuzzy = true,
    override_generic_sorter = true,
    override_file_sorter = true,
    case_mode = "smart_case",
  }

  telescope.setup({
    extensions = {
      fzf = fzf_opts,
      ["ui-select"] = {
        themes.get_dropdown({}),
      },
      live_grep_args = {
        auto_quoting = true,
        mappings = {
          i = {
            ["<C-k>"] = lga_actions.quote_prompt(),
            ["<C-i>"] = lga_actions.quote_prompt({ postfix = " --iglob " }),
          },
        },
      },
    },
    defaults = {
      file_ignore_patterns = { "^tags$", "^TAGS$", "^.git/", "^venv/" },
      mappings = {
        n = { ["<C-l>"] = trouble.open_with_trouble },
      },
      buffer_preview_maker = custom.new_maker,
    },
    pickers = {
      find_files = {
        -- find_command = { "rg", "--files", "--hidden", "-g", "!.git" },
        find_command = { "fd", "-t", "f", "--hidden", "--absolute-path", "--exclude", ".git", "--exclude", "venv" },
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
        mappings = {
          i = { ["<c-f>"] = actions.to_fuzzy_refine },
        },
      },
      lsp_dynamic_workspace_symbols = {
        sorter = telescope.extensions.fzf.native_fzf_sorter(fzf_opts),
      },
    },
  })

  pcall(telescope.load_extension, "aerial")
  pcall(telescope.load_extension, "dap")
  pcall(telescope.load_extension, "file_browser")
  pcall(telescope.load_extension, "frecency")
  pcall(telescope.load_extension, "fzf")
  pcall(telescope.load_extension, "live_grep_args")
  pcall(telescope.load_extension, "neoclip")
  pcall(telescope.load_extension, "possession")
  pcall(telescope.load_extension, "ui-select")
  pcall(telescope.load_extension, "undo")
end

return M
