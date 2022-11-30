if vim.fn.executable("rg") then
  vim.opt.grepprg = "rg --no-heading --hidden --vimgrep -g '!.git/'"
else
  vim.opt.grepprg = "git grep -in $*"
end
