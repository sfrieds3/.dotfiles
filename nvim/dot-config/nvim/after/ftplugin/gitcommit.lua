vim.opt_local.iskeyword:append({ "-" })

vim.api.nvim_create_autocmd("BufEnter", {
  callback = function()
    if #vim.api.nvim_list_wins() == 1 and vim.bo.filetype == "git" then
      vim.cmd("quit")
    end
  end,
  group = vim.api.nvim_create_augroup("gitcommit:closenvim", {}),
})
