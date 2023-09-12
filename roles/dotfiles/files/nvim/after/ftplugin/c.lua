local is_executable = vim.fn.executable
if is_executable("clang") == 1 then
  vim.bo.makeprg = clang
else
  vim.bo.makeprg = gcc
end

vim.bo.suffixesadd = ".h"

-- "let &l:errorformat="%f:%l:%c:\ %t%s:\ %m"

vim.bo.shiftwidth = 4
vim.bo.softtabstop = 4
