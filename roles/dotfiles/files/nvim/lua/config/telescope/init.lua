require("config.telescope.telescope_config")

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
      sort_lastused = true,
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

pcall(require("telescope").load_extension, "frecency")
pcall(require("telescope").load_extension, "fzf")
pcall(require("telescope").load_extension, "neoclip")
pcall(require("telescope").load_extension, "live_grep_args")
pcall(require("telescope").load_extension, "aerial")
pcall(require("telescope").load_extension, "ui-select")
pcall(require("telescope").load_extension, "project")
pcall(require("telescope").load_extension, "file_browser")
pcall(require("telescope").load_extension, "dap")

local map_telescope = function(key, cmd, theme, theme_config, mode)
  theme_config = theme_config or "previewer = false"
  mode = mode or "n"
  local base_command = "<Cmd> lua require('telescope.builtin').%s(require('telescope.themes').get_%s({s}))<cr>"
  local command = string.format(base_command, cmd, theme, theme_config)
  local desc = string.format("Telescope: %s", cmd)
  vim.keymap.set(mode, key, command, { desc = desc })
end

-- TODO: find all files (including ignored files)
vim.keymap.set(
  "n",
  "<Leader>sf",
  require("config.telescope.telescope_config").project_files,
  { desc = "Telescope: [s]earch project [f]iles" }
)
vim.keymap.set(
  "n",
  "<Leader>s?",
  require("config.telescope.telescope_config").old_files,
  { desc = "Telescope: old files" }
)
vim.keymap.set(
  "n",
  "<Leader>sr",
  require("config.telescope.telescope_config").recent_files,
  { desc = "Telescope: [s]earch [r]ecent files" }
)

map_telescope("<Leader>T", "lsp_dynamic_workspace_symbols", "ivy")
map_telescope("<Leader>t", "lsp_document_symbols", "ivy")
map_telescope("<Leader>sa", "aerial", "ivy")
map_telescope("<Leader><Leader>", "buffers", "ivy")
map_telescope("<Leader>;", "treesitter", "ivy")
map_telescope("gR", "grep_string", "ivy")
map_telescope("<Leader>vh", "search_history", "dropdown", "previewer = false")
map_telescope("<Leader>vc", "command_history", "dropdown", "previewer = false")
map_telescope("<Leader>vj", "jumplist", "dropdown")
map_telescope("<Leader>/", "current_buffer_fuzzy_find", "ivy")
map_telescope("<Leader>vm", "marks", "dropdown")
map_telescope("<Leader>vr", "registers", "dropdown")
map_telescope("<Leader>vh", "help_tags", "dropdown")
map_telescope("<Leader>vd", "diagnostics", "dropdown")

vim.keymap.set(
  "n",
  "<Leader>rr",
  require("config.telescope.telescope_config").preview_lsp_references,
  { desc = "[LSP] Telescope: preview lsp [r]eferences" }
)
vim.keymap.set(
  "n",
  "<Leader>vo",
  require("config.telescope.telescope_config").vim_options,
  { desc = "Telescope: [v]iew [o]ptions" }
)
vim.keymap.set(
  "n",
  "<Leader>vw",
  require("config.telescope.telescope_config").wiki_search,
  { desc = "Telescope: [v]iew [w]iki" }
)
vim.keymap.set(
  "n",
  "<Leader>gw",
  require("config.telescope.telescope_config").grep_wiki,
  { desc = "Telescope: [g]rep [w]iki" }
)
vim.keymap.set(
  "n",
  "<Leader>nn",
  require("config.telescope.telescope_config").edit_nvim_config,
  { desc = "Telescope: edit neovim config" }
)
vim.keymap.set(
  "n",
  "<Leader>gg",
  require("config.telescope.telescope_config").live_grep,
  { desc = "Telescope: live [g]rep" }
)
vim.keymap.set(
  "n",
  "<Leader>ga",
  require("config.telescope.telescope_config").live_grep_args,
  { desc = "Telescope: [g]rep [a]rgs" }
)
vim.keymap.set(
  "n",
  "<Leader>g/",
  require("config.telescope.telescope_config").grep_last_search,
  { desc = "Telescope: grep last search" }
)
vim.keymap.set("n", "<Leader>vp", require("telescope").extensions.neoclip.default, { desc = "Telescope: neoclip" })
vim.keymap.set("n", "<C-p>", require("telescope").extensions.project.project, { desc = "Telescope: select [p]roject" })
vim.keymap.set("n", "<Leader>vk", require("telescope.builtin").keymaps, { desc = "Telescope: [v]iew [k]eymaps" })

-- load local telescope config, if exists
pcall(require, "config.telescope.local")
