local Notes = {}

Notes.is_configured = false

Notes.config = {
  notes_dir = "~/wiki",
  notes_filenames = { "notes.md" },
}

--- Setup
---@param opts table setup configuration
function Notes.setup(opts)
  vim.tbl_deep_extend("force", Notes.config, opts)

  vim.keymap.set("n", "<leader>n", "<cmd>Notes<cr>", { desc = "Open Wiki Notes File" })
  Notes.is_configured = true
end

function Notes.open()
  local config = Notes.config

  local files =
    vim.fs.find(config.notes_filenames, { limit = math.huge, type = "file", path = vim.fn.expand(config.notes_dir) })

  require("fzf-lua").fzf_exec(files, { actions = {
    ["default"] = require("fzf-lua").actions.file_edit,
  } })
end

vim.api.nvim_create_user_command("Notes", function()
  require("sfrieds3.notes").open()
end, {})

if not Notes.is_configured then
  Notes.setup({})
end

return Notes
