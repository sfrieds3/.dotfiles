local M = {}

function M.init_lazy()
  local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
  if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
      "git",
      "clone",
      "--filter=blob:none",
      "--single-branch",
      "https://github.com/folke/lazy.nvim.git",
      lazypath,
    })
  end
  vim.opt.runtimepath:prepend(lazypath)
  require("lazy").setup("plugins", {
    install = { colorscheme = { "material", "ayu", "kanagawa", "habamax" } },
    dev = {
      path = "~/dev/personal",
      patterns = { "sfrieds3" },
    },
  })
end

return M
