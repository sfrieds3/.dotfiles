vim.o.grepformat = "%f:%l:%c:%m,%f:%l:%m"

if vim.fn.executable("rg") then
  vim.o.grepprg = "rg --vimgrep --smart-case $*"
elseif vim.fn.executable("ag") == 1 then
  vim.o.grepprg = "ag $*"
else
  vim.opt.grepprg = "git grep -in $*"
end
