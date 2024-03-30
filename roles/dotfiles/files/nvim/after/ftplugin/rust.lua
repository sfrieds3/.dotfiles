local bufnr = vim.api.nvim_get_current_buf()

vim.bo.makeprg = "cargo clippy"
-- vim.bo.compiler = "cargo"

vim.api.nvim_create_autocmd("BufWritePre", {
  callback = function(args)
    require("conform").format({ lsp_fallback = true, bufnr = args.bufnr })
  end,
  buffer = bufnr,
  group = vim.api.nvim_create_augroup("jsonformat:" .. bufnr, {}),
})

require("plugins.test.rust").setup()
