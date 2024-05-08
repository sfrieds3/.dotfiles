local augroup = require("utils.utils").augroup

vim.api.nvim_create_autocmd("InsertEnter", { command = "set nolist", group = augroup("nolist_insertenter") })
vim.api.nvim_create_autocmd("InsertLeave", { command = "set list", group = augroup("list_insertleave") })
vim.api.nvim_create_autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = augroup("highlight_on_yank"),
})

vim.api.nvim_create_autocmd("TermOpen", {
  pattern = "term://*",
  command = [[setlocal listchars= nonumber norelativenumber | startinsert]],
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
    "oil",
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

local term_map = augroup("TermMap")
vim.api.nvim_create_autocmd({ "TermEnter" }, {
  group = term_map,
  pattern = "term://*",
  callback = function()
    local buf_keymap_opts = { buffer = true }
    vim.keymap.set("t", '""', [[<C-\><C-n>]], buf_keymap_opts)
    --vim.keymap.set('t', '<M-h>', [[<C-\><C-n><C-w>h]], buf_keymap_opts)
    --vim.keymap.set('t', '<M-j>', [[<C-\><C-n><C-w>j]], buf_keymap_opts)
    --vim.keymap.set('t', '<M-k>', [[<C-\><C-n><C-w>k]], buf_keymap_opts)
    --vim.keymap.set('t', '<M-l>', [[<C-\><C-n><C-w>l]], buf_keymap_opts)
  end,
})
