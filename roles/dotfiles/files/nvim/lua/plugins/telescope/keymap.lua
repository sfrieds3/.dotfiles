local M = {}
local telescope = require("telescope")
local builtin = require("telescope.builtin")
local themes = require("telescope.themes")
local custom = require("plugins.telescope.custom")
local multi_rg = require("plugins.telescope.multi_rg")
local utils = require("utils.utils")

local map_telescope = function(mode, key, cmd, theme, config)
  config = config or { "previewer = false" }
  mode = mode or "n"
  local desc = utils.remove_key(config, "desc") or ""

  -- TODO: support default theme
  local theme_funcs = {
    dropdown = themes.get_dropdown,
    ivy = themes.get_ivy,
    cursor = themes.get_cursor,
  }
  vim.keymap.set(mode, key, function()
    cmd(theme_funcs[theme](config))
  end, { desc = "Telescope: " .. desc })
end

function M.set_keymap()
  -- TODO: display LSP diagnostics from changed files (i.e. files changed in git)
  vim.keymap.set("n", "<Leader>.", builtin.resume, { desc = "Telescope: resume" })
  vim.keymap.set("n", "<Leader>sf", custom.project_files, { desc = "Telescope: [s]earch project [f]iles" })
  vim.keymap.set("n", "<Leader>s?", custom.old_files, { desc = "Telescope: old files" })
  vim.keymap.set("n", "<Leader>sr", custom.recent_files, { desc = "Telescope: [s]earch [r]ecent files" })
  vim.keymap.set("n", "<Leader>u", telescope.extensions.undo.undo, { desc = "Telescope: [u]ndo" })

  map_telescope("n", "<Leader>sa", builtin.aerial, "ivy", { desc = "aerial" }) -- TODO: this doesn't work..
  map_telescope("n", "<Leader><Leader>", builtin.buffers, "ivy", { desc = "buffers" })
  map_telescope("n", "<Leader>gr", builtin.grep_string, "ivy", { desc = "[g][r]ep string" })
  map_telescope(
    "n",
    "<Leader>vh",
    builtin.search_history,
    "dropdown",
    { desc = "[v]iew search [h]istory", previewer = false }
  )
  map_telescope(
    "n",
    "<Leader>vc",
    builtin.command_history,
    "dropdown",
    { desc = "[v]iew [c]ommand history", previewer = false }
  )
  map_telescope(
    "n",
    "<Leader>vj",
    builtin.jumplist,
    "dropdown",
    { desc = "[v]iew [j]umplist", winblend = 10, layout_config = { width = 0.50 } }
  )
  map_telescope("n", "<Leader>/", builtin.current_buffer_fuzzy_find, "ivy", { desc = "current buffer fuzzy find" })
  map_telescope(
    "n",
    "<Leader>vm",
    builtin.marks,
    "dropdown",
    { desc = "[v]iew [m]arks", winblend = 10, layout_config = { width = 0.50 } }
  )
  map_telescope(
    "n",
    "<Leader>vr",
    builtin.registers,
    "dropdown",
    { desc = "[v]iew [r]egisters", winblend = 10, layout_config = { width = 0.50 } }
  )
  map_telescope(
    "n",
    "<Leader>vh",
    builtin.help_tags,
    "dropdown",
    { desc = "[v]iew [h]elptags", winblend = 10, layout_config = { width = 0.50 } }
  )
  map_telescope(
    "n",
    "<Leader>vd",
    builtin.diagnostics,
    "dropdown",
    { desc = "[v]iew [d]iagnostics", winblend = 10, layout_config = { width = 0.75 } }
  )
  map_telescope("n", "<Leader>vq", builtin.quickfix, "ivy", { desc = "[v]iew [q]uickfix" })
  map_telescope("n", "<Leader>vl", builtin.loclist, "ivy", { desc = "[v]iew [l]oclist" })

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
    "<Leader>gR",
    custom.preview_lsp_references,
    { desc = "[LSP] Telescope: [g]oto lsp [R]eferences" }
  )
  vim.keymap.set(
    "n",
    "<Leader>gD",
    custom.preview_lsp_definitions,
    { desc = "[LSP] Telescope: [g]oto lsp [D]efinitions" }
  )
  vim.keymap.set(
    "n",
    "<Leader>I",
    custom.preview_lsp_incoming_calls,
    { desc = "[LSP] Telescope: lsp [I]ncomming calls" }
  )
  vim.keymap.set(
    "n",
    "<Leader>O",
    custom.preview_lsp_outgoing_calls,
    { desc = "[LSP] Telescope: lsp [O]utgiong calls" }
  )
  vim.keymap.set("n", "<Leader>vo", custom.vim_options, { desc = "Telescope: [v]iew [o]ptions" })
  vim.keymap.set("n", "<Leader>vw", custom.wiki_search, { desc = "Telescope: [v]iew [w]iki" })
  vim.keymap.set("n", "<Leader>gw", custom.grep_wiki, { desc = "Telescope: [g]rep [w]iki" })
  vim.keymap.set("n", "<Leader>nn", custom.edit_nvim_config, { desc = "Telescope: edit neovim config" })
  vim.keymap.set("n", "<Leader>gg", custom.live_grep, { desc = "Telescope: live [g]rep" })
  vim.keymap.set("n", "<Leader>rg", multi_rg, { desc = "Telescope: multi [r]ip[g]rep" })
  vim.keymap.set("n", "<Leader>gp", custom.live_grep_preview, { desc = "Telescope: live [g]rep [p]review" })
  vim.keymap.set("n", "<Leader>ga", custom.live_grep_args, { desc = "Telescope: [g]rep [a]rgs" })
  vim.keymap.set("n", "<Leader>g/", custom.grep_last_search, { desc = "Telescope: grep last search" })
  vim.keymap.set("n", "<Leader>vp", telescope.extensions.neoclip.default, { desc = "Telescope: neoclip" })
  vim.keymap.set("n", "<Leader>vk", builtin.keymaps, { desc = "Telescope: [v]iew [k]eymaps" })
  vim.keymap.set("n", "<Leader>si", custom.installed_plugins, { desc = "Telescope: [s]earch [i]nstalled plugins" })
  vim.keymap.set("n", "<Leader>gi", custom.grep_installed_plugins, { desc = "Telescope: [g]rep [i]nstalled plugins" })

  -- load local telescope config, if exists
  pcall(require, "config.telescope.local")
end

return M
