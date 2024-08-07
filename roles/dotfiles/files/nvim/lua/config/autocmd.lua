local augroup = require("utils.utils").augroup

vim.api.nvim_create_autocmd("InsertEnter", { command = "set nolist", group = augroup("nolist_insertenter") })
vim.api.nvim_create_autocmd("InsertLeave", { command = "set list", group = augroup("list_insertleave") })
vim.api.nvim_create_autocmd("TextYankPost", {
  group = augroup("highlight_on_yank"),
  callback = function()
    vim.highlight.on_yank()
  end,
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

vim.api.nvim_create_autocmd({ "FileType" }, {
  group = augroup("q-filetypes"),
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
    "oil",
    "qf",
    "spectre_panel",
    "startuptime",
    "tsplayground",
    "gitsigns-blame",
  },
  callback = function(event)
    vim.bo[event.buf].buflisted = false
    vim.keymap.set("n", "q", "<Cmd>close<CR>", { buffer = event.buf, silent = true })
  end,
})

local disable_list_ft = { "go" }
vim.api.nvim_create_autocmd("BufEnter", {
  desc = "Disable listchars for certain ft",
  group = augroup("nolist_bufenter"),
  callback = function(t)
    if vim.tbl_contains(disable_list_ft, vim.bo[t.buf].filetype) then
      vim.opt.list = false
    end
  end,
})
