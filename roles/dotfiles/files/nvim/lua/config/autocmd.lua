local function augroup(name, check_dupe)
  local group_name = "sfrieds3:" .. name
  local should_check_dupe = check_dupe or true
  if should_check_dupe then
    local err, _ = pcall(vim.api.nvim_get_autocmds, { group = group_name })
    if err then
      print("augroup ", group_name, " already exists, bailing.")
      return
    end
  end

  return vim.api.nvim_create_augroup(group_name, { clear = true })
end

vim.api.nvim_create_autocmd("InsertEnter", { command = "set nolist", group = augroup("nolist_insertenter") })
vim.api.nvim_create_autocmd("InsertLeave", { command = "set list", group = augroup("list_insertleave") })
vim.api.nvim_create_autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = augroup("highlight_on_yank"),
})

vim.api.nvim_create_autocmd({ "QuickFixCmdPost" }, {
  group = augroup("autoquickfix"),
  pattern = "[^l]*",
  command = [[cwindow]],
})
vim.api.nvim_create_autocmd({ "QuickFixCmdPost" }, {
  group = augroup("autoloclist"),
  pattern = "l*",
  command = [[lwindow]],
})

local exclude_filetypes = { ["neo-tree"] = true }
vim.api.nvim_create_autocmd({ "WinEnter" }, {
  group = augroup("cursorline_winenter"),
  callback = function()
    vim.wo.cursorline = true
  end,
})
vim.api.nvim_create_autocmd({ "WinLeave" }, {
  group = augroup("nocursorline_winleave"),
  callback = function()
    if not exclude_filetypes[vim.bo.filetype] then
      vim.wo.cursorline = false
    end
  end,
})

-- enable TSContext when there is no LSP client
-- vim.api.nvim_create_autocmd({ "BufEnter" }, {
--   group = vim.api.nvim_create_augroup("TSContextAutocmd", { clear = true }),
--   pattern = "*",
--   callback = function()
--     local status, is_available = pcall(require("nvim-navic").is_available)
--     if status and is_available then
--       ---@diagnostic disable-next-line
--       local status, _ = pcall(vim.cmd, "TSContextDisable")
--     else
--       ---@diagnostic disable-next-line
--       local staus, _ = pcall(vim.cmd, "TSContextEnable")
--     end
--   end,
-- })

vim.api.nvim_create_autocmd({ "FileType" }, {
  group = augroup("q_filetypes"),
  pattern = {
    "Outline",
    "PlenaryTestPopup",
    "checkhealth",
    "fugitiveblame",
    "git",
    "help",
    "lspinfo",
    "man",
    "neotest-output",
    "neotest-output-panel",
    "neotest-summary",
    "notify",
    "qf",
    "spectre_panel",
    "startuptime",
    "tsplayground",
  },
  callback = function(event)
    vim.bo[event.buf].buflisted = false
    vim.keymap.set("n", "q", "<Cmd>close<CR>", { buffer = event.buf, silent = true })
  end,
})
