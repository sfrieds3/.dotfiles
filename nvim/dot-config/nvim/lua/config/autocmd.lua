local augroup = require("utils.utils").augroup

vim.api.nvim_create_autocmd("InsertEnter", { command = "set nolist", group = augroup("nolist_insertenter") })
vim.api.nvim_create_autocmd("InsertLeave", { command = "set list", group = augroup("list_insertleave") })
vim.api.nvim_create_autocmd("TextYankPost", {
  group = augroup("highlight_on_yank"),
  callback = function()
    vim.hl.on_yank()
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

vim.api.nvim_create_autocmd("FileType", {
  group = augroup("qf-filter"),
  pattern = "qf",
  callback = function(event)
    vim.keymap.set("n", "zf", function()
      require("snacks").picker.qflist()
    end, { buffer = event.buf, silent = true })
  end,
})

local disable_list_ft = { "go" }
vim.api.nvim_create_autocmd("BufEnter", {
  desc = "Disable listchars for certain ft",
  group = augroup("nolist-bufenter"),
  callback = function(t)
    if vim.tbl_contains(disable_list_ft, vim.bo[t.buf].filetype) then
      vim.opt.list = false
    end
  end,
})

vim.api.nvim_create_autocmd("VimEnter", {
  callback = function()
    if vim.wo.diff then
      vim.keymap.set({ "n", "v" }, "<localleader>dl", ":diffget LOCAL<CR>", { buffer = true, desc = "Get from LOCAL" })
      vim.keymap.set(
        { "n", "x" },
        "<localleader>dr",
        ":diffget REMOTE<CR>",
        { buffer = true, desc = "Get from REMOTE" }
      )

      vim.keymap.set(
        { "n", "x" },
        "gdb",
        ":diffget BASE<CR>:diffget REMOTE<CR>",
        { buffer = true, desc = "Accept both" }
      )
    end
  end,
})

local function map(tbl, func)
  local return_table = {}
  for x, y in pairs(tbl) do
    return_table[x] = func(y)
  end
  return return_table
end

-- local proj_dirs = { "~/code" }
-- local format_func = function(d)
--   return string.format("%s/*", vim.fn.expand(d))
-- end
-- vim.api.nvim_create_autocmd("BufEnter", {
--   desc = "Set files not in project to `modifiable` = `false`",
--   pattern = map(proj_dirs, format_func),
--   group = augroup("set-proj-modifiable"),
--   callback = function(t)
--     local file_path = vim.fn.expand(t.file)
--
--     if file_path == "" or vim.fn.empty(vim.fn.bufname()) == 1 then
--       return
--     end
--
--     if vim.fn.match(file_path, ".*/.git/.*") ~= -1 then
--       return
--     end
--
--     vim.system(
--       { "git", "ls-files", "--cached", "--others", "--error-unmatch", "--exclude-standard", file_path },
--       {},
--       function(out)
--         vim.schedule(function()
--           if out.code == 1 then
--             vim.opt_local.modifiable = false
--           end
--         end)
--       end
--     )
--   end,
-- })
