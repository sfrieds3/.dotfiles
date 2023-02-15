local M = {
  "ludovicchabant/vim-gutentags",
  enabled = false,
  event = "BufReadPost",
}

function M.config()
  vim.g.gutentags_cache_dir = vim.env.XDG_CACHE_HOME .. "/tags"
  if vim.fn.has("macunix") then
    vim.g.gutentags_ctags_executable = "/opt/homebrew/bin/ctags"
  end
end

return M
