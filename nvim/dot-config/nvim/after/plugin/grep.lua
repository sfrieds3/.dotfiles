if vim.fn.executable("rg") then
  vim.o.grepprg = "rg --no-heading --smart-case --vimgrep $*"
  vim.o.grepformat = "%f:%l:%c:%m,%f:%l:%m"
elseif vim.fn.executable("ag") == 1 then
  vim.o.grepprg = "ag --vimgrep $*"
  vim.o.grepformat = "%f:%l:%c:%m"
else
  vim.opt.grepprg = "git grep -in $*"
end
