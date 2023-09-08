local is_executable = vim.fn.executable
local bufnr = vim.api.nvim_get_current_buf()

vim.bo.shiftwidth = 2
vim.bo.softtabstop = 2
vim.bo.expandtab = true

if is_executable("stylua") then
  vim.api.nvim_create_autocmd("BufWritePost", {
    callback = function(args)
      require("conform").format({ formatters = { "stylua" }, bufnr = args.bufnr })
    end,
    buffer = bufnr,
    group = vim.api.nvim_create_augroup("stylua:" .. bufnr, {}),
  })
end
