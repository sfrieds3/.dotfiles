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
      find_command = { "rg", "--files", "--hidden", "-g", "!.git" },
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

local map_telescope = function(key, cmd, theme, theme_config, mode)
  theme_config = theme_config or "previewer = false"
  mode = mode or "n"
  local base_command = "<cmd> lua require('telescope.builtin').%s(require('telescope.themes').get_%s({s}))<cr>"
  local command = string.format(base_command, cmd, theme, theme_config)
  vim.keymap.set(mode, key, command)
end

vim.keymap.set("n", "<Leader>f", "<cmd>lua require('config.telescope.telescope_config').project_files()<CR>")
vim.keymap.set("n", "<Leader>o", "<cmd>lua require('config.telescope.telescope_config').old_files()<CR>")
vim.keymap.set("n", "<Leader>r", "<cmd>lua require('config.telescope.telescope_config').recent_files()<CR>")
vim.keymap.set("n", "<Leader>Tt", "<cmd>lua require('config.telescope.telescope_config').buffer_tags()<CR>")
vim.keymap.set("n", "<Leader>TT", "<cmd>lua require('config.telescope.telescope_config').project_tags()<CR>")

map_telescope("<Leader>b", "buffers", "ivy")
map_telescope("<Leader><Space>", "oldfiles", "ivy")
map_telescope("<Leader>tt", "treesitter", "ivy")
map_telescope("<Leader>tk", "keymaps", "dropdown")
map_telescope("<Leader>gr", "grep_string", "dropdown")
map_telescope("<Leader>ts", "search_history", "dropdown", "previewer = false")
map_telescope("<Leader>tc", "command_history", "dropdown", "previewer = false")
map_telescope("<Leader>tj", "jumplist", "dropdown")
map_telescope("<Leader>tl", "lsp_document_symbols", "ivy")
map_telescope("<Leader>t<Space>", "current_buffer_fuzzy_find", "ivy")
map_telescope("<Leader>tm", "marks", "dropdown")
map_telescope("<Leader>tr", "registers", "dropdown")
map_telescope("<Leader>th", "help_tags", "dropdown")
map_telescope("<Leader>td", "diagnostics", "dropdown")

vim.keymap.set("n", "<Leader>tv", "<Cmd>lua require('config.telescope.telescope_config').vim_options()<CR>")
vim.keymap.set("n", "<Leader>tw", "<Cmd>lua require('config.telescope.telescope_config').wiki_search()<CR>")
vim.keymap.set("n", "<Leader>tn", "<Cmd>lua require('config.telescope.telescope_config').edit_nvim_config()<CR>")
vim.keymap.set("n", "<Leader>gg", "<Cmd>lua require('config.telescope.telescope_config').live_grep()<CR>")
vim.keymap.set("n", "<Leader>tgw", "<Cmd>lua require('config.telescope.telescope_config').grep_wiki()<CR>")
vim.keymap.set("n", "<Leader>tgg", "<Cmd>lua require('config.telescope.telescope_config').live_grep_args()<CR>")
vim.keymap.set("n", "<Leader>t/", "<Cmd>lua require('config.telescope.telescope_config').grep_last_search()<CR>")
vim.keymap.set("n", "<Leader>tp", "<Cmd>lua require('telescope').extensions.neoclip.default()<CR>")

-- load local telescope config, if exists
pcall(require, "config.telescope.local")
