local augroup = require("utils.utils").augroup

vim.bo.shiftwidth = 2
vim.bo.softtabstop = 2
vim.bo.tabstop = 2

vim.api.nvim_create_autocmd("BufWritePre", {
  callback = function(args)
    require("conform").format({ formatters = { "gofmt", "goimports" }, bufnr = args.bufnr })
  end,
  buffer = vim.api.nvim_get_current_buf(),
  group = augroup("goformat:" .. vim.api.nvim_get_current_buf()),
})

vim.bo.makeprg = "go run %"

local augroup_name = "sfrieds3:golang_runonsave"
vim.api.nvim_buf_create_user_command(vim.api.nvim_get_current_buf(), "GoRunOnSaveEnable", function()
  vim.api.nvim_create_autocmd("BufWritePost", {
    group = vim.api.nvim_create_augroup(augroup_name, { clear = true }),
    callback = function()
      vim.cmd("make")
    end,
  })
end, {})

vim.api.nvim_buf_create_user_command(vim.api.nvim_get_current_buf(), "GoRunOnSaveDisable", function()
  vim.api.nvim_del_augroup_by_name(augroup_name)
end, {})
