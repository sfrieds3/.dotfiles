local telescope_config = require("config.telescope.telescope_config")

require("telescope").setup({
  extensions = {
    fzf = {
      fuzzy = true,
      override_generic_sorter = true,
      override_file_sorter = true,
      case_mode = "smart_case",
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
    },
    live_grep = {
      previewer = false,
      theme = "dropdown",
    },
  },
})

require("telescope").load_extension("frecency")
require("telescope").load_extension("fzf")
require("telescope").load_extension("neoclip")
require("telescope").load_extension("live_grep_args")
require("telescope").load_extension("aerial")

local map_telescope = function(key, cmd, theme, theme_config, mode)
  theme_config = theme_config or "previewer = false"
  mode = mode or "n"
  local base_command = "<cmd> lua require('telescope.builtin').%s(require('telescope.themes').get_%s({s}))<cr>"
  local command = string.format(base_command, cmd, theme, theme_config)
  vim.keymap.set(mode, key, command)
end

vim.keymap.set("n", "<Leader>f", "<cmd>lua require('config.telescope.telescope_config').project_files()<CR>")
vim.keymap.set("n", "<Leader>o", "<cmd>lua require('config.telescope.telescope_config').old_files()<CR>")
vim.keymap.set("n", "<Leader>e", "<cmd>lua require('config.telescope.telescope_config').recent_files()<CR>")

map_telescope("<Leader>T", "lsp_dynamic_workspace_symbols", "ivy")
map_telescope("<Leader>t", "lsp_document_symbols", "ivy")
map_telescope("<Leader>s", "aerial", "ivy")
map_telescope("<Leader>b", "buffers", "ivy")
map_telescope("<Leader>;", "treesitter", "ivy")
map_telescope("<Leader>k", "keymaps", "dropdown")
map_telescope("<Leader>gr", "grep_string", "ivy")
map_telescope("<Leader>s", "search_history", "dropdown", "previewer = false")
map_telescope("<Leader>c", "command_history", "dropdown", "previewer = false")
map_telescope("<Leader>j", "jumplist", "dropdown")
map_telescope("<Leader>'", "current_buffer_fuzzy_find", "ivy")
map_telescope("<Leader>vm", "marks", "dropdown")
map_telescope("<Leader>vr", "registers", "dropdown")
map_telescope("<Leader>vh", "help_tags", "dropdown")
map_telescope("<Leader>vd", "diagnostics", "dropdown")

vim.keymap.set("n", "<Leader>vo", "<Cmd>lua require('config.telescope.telescope_config').vim_options()<CR>")
vim.keymap.set("n", "<Leader>vw", "<Cmd>lua require('config.telescope.telescope_config').wiki_search()<CR>")
vim.keymap.set("n", "<Leader>nn", "<Cmd>lua require('config.telescope.telescope_config').edit_nvim_config()<CR>")
vim.keymap.set("n", "<Leader>gg", "<Cmd>lua require('config.telescope.telescope_config').live_grep()<CR>")
vim.keymap.set("n", "<Leader>gw", "<Cmd>lua require('config.telescope.telescope_config').grep_wiki()<CR>")
vim.keymap.set("n", "<Leader>ga", "<Cmd>lua require('config.telescope.telescope_config').live_grep_args()<CR>")
vim.keymap.set("n", "<Leader>g/", "<Cmd>lua require('config.telescope.telescope_config').grep_last_search()<CR>")
vim.keymap.set("n", "<Leader>p", "<Cmd>lua require('telescope').extensions.neoclip.default()<CR>")

-- load local telescope config, if exists
pcall(require, "config.telescope.local")
