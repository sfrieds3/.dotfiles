local augroup = require("utils.utils").augroup

vim.bo.shiftwidth = 2

-- TODO: call module name here
vim.bo.makeprg = "dune build"

vim.api.nvim_create_autocmd("BufWritePre", {
  callback = function(args)
    require("conform").format({ formatters = { "ocamlformat" }, bufnr = args.bufnr })
  end,
  buffer = vim.api.nvim_get_current_buf(),
  group = augroup("ocamlformat:" .. vim.api.nvim_get_current_buf()),
})
