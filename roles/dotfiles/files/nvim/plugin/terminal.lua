local augroup = require("utils.utils").augroup

vim.api.nvim_create_autocmd("TermOpen", {
  group = augroup("term-open"),
  callback = function()
    vim.opt_local.number = false
    vim.opt_local.relativenumber = false
    vim.opt_local.listchars = {}
    vim.opt_local.scrolloff = 0
    vim.cmd([[startinsert]])
  end,
})

vim.api.nvim_create_autocmd({ "TermEnter" }, {
  group = augroup("term-map"),
  callback = function()
    local buf_keymap_opts = { buffer = true }
    vim.keymap.set("t", '""', [[<C-\><C-n>]], buf_keymap_opts)
  end,
})
