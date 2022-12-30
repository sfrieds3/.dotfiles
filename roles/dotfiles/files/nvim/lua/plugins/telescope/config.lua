local M = {}

function M.setup()
  local telescope = require("telescope")
  local builtin = require("telescope.builtin")
  local themes = require("telescope.themes")
  local custom = require("plugins.telescope.custom")

  require("telescope").setup({
    extensions = {
      ["fzf"] = {
        fuzzy = true,
        override_generic_sorter = true,
        override_file_sorter = true,
        case_mode = "smart_case",
      },
      ["ui-select"] = {
        require("telescope.themes").get_dropdown({}),
      },
      ["project"] = {
        base_dirs = {
          "$HOME/.dotfiles",
          { "$HOME/dev", max_depth = 4 },
        },
      },
    },
    defaults = {
      file_ignore_patterns = { "tags", "TAGS", "^.git/" },
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
        previewer = false,
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
  pcall(telescope.load_extension, "project")
  pcall(telescope.load_extension, "file_browser")
  pcall(telescope.load_extension, "dap")

  local map_telescope = function(key, cmd, theme, theme_config, mode)
    theme_config = theme_config or { "previewer = false" }
    mode = mode or "n"

    -- TODO: figure out how to suppport default theme
    local theme_funcs = {
      dropdown = themes.get_dropdown,
      ivy = themes.get_ivy,
      cursor = themes.get_cursor,
    }
    vim.keymap.set(mode, key, function()
      cmd(theme_funcs[theme](theme_config))
    end, { desc = "Telescope: " })
  end

  -- TODO: find all files (including ignored files)
  vim.keymap.set("n", "<Leader>.", builtin.resume, { desc = "Telescope: resume" })
  vim.keymap.set("n", "<Leader>sf", custom.project_files, { desc = "Telescope: [s]earch project [f]iles" })
  vim.keymap.set("n", "<Leader>s?", custom.old_files, { desc = "Telescope: old files" })
  vim.keymap.set("n", "<Leader>sr", custom.recent_files, { desc = "Telescope: [s]earch [r]ecent files" })

  map_telescope("<Leader>sa", builtin.aerial, "ivy") -- TODO: this doesn't work..
  map_telescope("<Leader><Leader>", builtin.buffers, "ivy")
  map_telescope("<Leader>gr", builtin.grep_string, "ivy")
  map_telescope("<Leader>vh", builtin.search_history, "dropdown", { "previewer = false" })
  map_telescope("<Leader>vc", builtin.command_history, "dropdown", { "previewer = false" })
  map_telescope("<Leader>vj", builtin.jumplist, "dropdown")
  map_telescope("<Leader>/", builtin.current_buffer_fuzzy_find, "ivy")
  map_telescope("<Leader>vm", builtin.marks, "dropdown")
  map_telescope("<Leader>vr", builtin.registers, "dropdown")
  map_telescope("<Leader>vh", builtin.help_tags, "dropdown")
  map_telescope("<Leader>vd", builtin.diagnostics, "dropdown", { winblend = 10, layout_config = { width = 0.75 } })
  map_telescope("<Leader>vq", builtin.quickfix, "ivy")
  map_telescope("<Leader>vl", builtin.loclist, "ivy")

  vim.keymap.set(
    "n",
    "<Leader>:",
    custom.workspace_symbols,
    { desc = "Telescope: Get workspace symbols from active LSP client" }
  )
  vim.keymap.set(
    "n",
    "<Leader>;",
    custom.document_symbols,
    { desc = "Telescope: LSP document symbols if attached to LSP client, else treesitter symbols" }
  )
  vim.keymap.set(
    "n",
    "<Leader>lr",
    custom.preview_lsp_references,
    { desc = "[LSP] Telescope: preview [l]sp [r]eferences" }
  )
  vim.keymap.set(
    "n",
    "<Leader>ld",
    custom.preview_lsp_definitions,
    { desc = "[LSP] Telescope: preview [l]sp [d]efinitions" }
  )
  vim.keymap.set("n", "<Leader>vo", custom.vim_options, { desc = "Telescope: [v]iew [o]ptions" })
  vim.keymap.set("n", "<Leader>vw", custom.wiki_search, { desc = "Telescope: [v]iew [w]iki" })
  vim.keymap.set("n", "<Leader>gw", custom.grep_wiki, { desc = "Telescope: [g]rep [w]iki" })
  vim.keymap.set("n", "<Leader>nn", custom.edit_nvim_config, { desc = "Telescope: edit neovim config" })
  vim.keymap.set("n", "<Leader>gg", custom.live_grep, { desc = "Telescope: live [g]rep" })
  vim.keymap.set("n", "<Leader>ga", custom.live_grep_args, { desc = "Telescope: [g]rep [a]rgs" })
  vim.keymap.set("n", "<Leader>g/", custom.grep_last_search, { desc = "Telescope: grep last search" })
  vim.keymap.set("n", "<Leader>vp", require("telescope").extensions.neoclip.default, { desc = "Telescope: neoclip" })
  vim.keymap.set(
    "n",
    "<C-p>",
    require("telescope").extensions.project.project,
    { desc = "Telescope: select [p]roject" }
  )
  vim.keymap.set("n", "<Leader>vk", require("telescope.builtin").keymaps, { desc = "Telescope: [v]iew [k]eymaps" })

  -- load local telescope config, if exists
  pcall(require, "config.telescope.local")
end

return M
