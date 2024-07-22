if FORCE_RELOAD then
  package.loaded["sfrieds3.notes"] = nil
end

local Notes = {}

Notes.force_reload = true
Notes.is_configured = false

Notes.config = {
  notes_dir = "~/wiki",
  notes_filenames = { "notes.md" },
}

function Notes.get_float_fullscreen_opts()
  local win_width = vim.api.nvim_win_get_width(0)
  local win_height = vim.api.nvim_win_get_height(0)
  local row = 0
  local col = 0

  return {
    relative = "win",
    width = vim.fn.round(win_width * 0.75),
    height = vim.fn.round(win_height * 0.75),
    row = (win_height - (win_height * 0.75)) / 2,
    col = (win_width - (win_width * 0.75)) / 2,
    border = "rounded",
  }
end

--- Setup
---@param opts table setup configuration
function Notes.setup(opts)
  vim.tbl_deep_extend("force", Notes.config, opts)

  vim.keymap.set("n", "<leader>n", "<cmd>Notes<cr>", { desc = "Open Wiki Notes File" })
  Notes.is_configured = true
end

function Notes.open(opts)
  opts = opts or {}
  local floating = opts["floating"] or false
  local config = Notes.config

  local files =
    vim.fs.find(config.notes_filenames, { limit = math.huge, type = "file", path = vim.fn.expand(config.notes_dir) })

  -- TODO: support floating window
  -- something like: `vim.api.nvim_open_win(0, 0, {relative='cursor', width=vim.api.nvim_win_get_width(0), height=vim.api.nvim_win_get_height(0), row=0, col=0, style='minimal'})
  if floating then
    local original_winid = vim.api.nvim_get_current_win()
    local win_opts = Notes.get_float_fullscreen_opts()
    local bufnr = vim.api.nvim_create_buf(false, true)
    local winid = vim.api.nvim_open_win(bufnr, true, win_opts)
    require("fzf-lua").fzf_exec(files, {
      actions = {
        ["default"] = function(selected)
          vim.bo[bufnr].bufhidden = "wipe"
          vim.cmd.edit({ args = selected })
        end,
      },
    })
  else
    require("fzf-lua").fzf_exec(files, { actions = {
      ["default"] = require("fzf-lua").actions.file_edit,
    } })
  end
end

vim.api.nvim_create_user_command("Notes", function()
  require("sfrieds3.notes").open()
end, {})

vim.api.nvim_create_user_command("NotesFloating", function()
  require("sfrieds3.notes").open({ floating = true })
end, {})

if not Notes.is_configured or Notes.force_reload then
  Notes.setup({})
end

return Notes
