local M = {}

local nvim_personal_plugins = vim.env.NVIM_PERSONAL_PLUGINS or "~/code/personal"

function M.init_lazy()
  local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
  if not (vim.uv or vim.loop).fs_stat(lazypath) then
    local lazyrepo = "https://github.com/folke/lazy.nvim.git"
    vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  end

  vim.opt.runtimepath:prepend(lazypath)
  require("lazy").setup({
    spec = { import = "plugins" },
    install = { colorscheme = { "ghostty-default", "habamax" } },
    dev = {
      path = nvim_personal_plugins,
      patterns = { "sfrieds3" },
    },
    change_detection = {
      enabled = true,
      notify = false,
    },
    diff = {
      cmd = "git",
    },
    pkg = {
      enabled = true,
      cache = vim.fn.stdpath("state") .. "/lazy/pkg-cache.lua",
      sources = {
        "lazy",
        "rockspec",
        "packspec",
      },
    },
    rocks = {
      root = vim.fn.stdpath("data") .. "/lazy-rocks",
      server = "https://nvim-neorocks.github.io/rocks-binaries/",
    },
  })
end

return M
