-- nvim autocmd group
vim.api.nvim_create_augroup("Neovim", { clear = true })
vim.api.nvim_create_autocmd("InsertEnter", { command = "set nolist", group = "Neovim" })
vim.api.nvim_create_autocmd("InsertLeave", { command = "set list", group = "Neovim" })
vim.api.nvim_create_autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = "Neovim",
})

vim.api.nvim_create_augroup("Winbar", { clear = true })
vim.api.nvim_create_autocmd({ "BufWinEnter", "BufFilePost", "InsertEnter", "BufWritePost" }, {
  group = "Winbar",
  callback = function()
    require("scwfri.winbar").get_winbar()
  end,
})

vim.api.nvim_create_augroup("NvimStartup", { clear = true })
vim.api.nvim_create_autocmd({ "VimEnter" }, {
  group = "NvimStartup",
  callback = function() end,
})

-- vim.api.nvim_create_augroup("PackerReload", { clear = true })
-- vim.api.nvim_create_autocmd({ "BufWritePost" }, {
--   group = "PackerReload",
--   pattern = "plugins.lua",
--   callback = function(args)
--     local cmd = "source " .. args.file .. " | PackerCompile"
--     vim.cmd(cmd)
--   end,
-- })

-- open quickfix or location-list automatically when there is something to show
-- source: https://gist.github.com/romainl/56f0c28ef953ffc157f36cc495947ab3
vim.api.nvim_create_augroup("AutoQuickfix", { clear = true })
vim.api.nvim_create_autocmd({ "QuickFixCmdPost" }, {
  group = "AutoQuickfix",
  pattern = "[^l]*",
  command = [[cwindow]],
})
vim.api.nvim_create_autocmd({ "QuickFixCmdPost" }, {
  group = "AutoQuickfix",
  pattern = "l*",
  command = [[lwindow]],
})

vim.api.nvim_create_augroup("WindowCursorLine", { clear = true })
vim.api.nvim_create_autocmd({ "WinEnter" }, {
  group = "WindowCursorLine",
  pattern = "*",
  command = [[set cursorline]],
})
vim.api.nvim_create_autocmd({ "WinLeave" }, {
  group = "WindowCursorLine",
  pattern = "*",
  command = [[set nocursorline]],
})

-- vim.api.nvim_create_augroup("LspAutocmd", { clear = true })
-- vim.api.nvim_create_autocmd({ "CursorHold" }, {
--   group = "LspAutocmd",
--   pattern = "*",
--   callback = function()
--     vim.diagnostic.open_float({ focusable = false })
--   end,
-- })

-- enable TSContext when there is no LSP client
vim.api.nvim_create_autocmd({ "BufEnter" }, {
  group = vim.api.nvim_create_augroup("TSContextAutocmd", { clear = true }),
  pattern = "*",
  callback = function()
    local status, is_available = pcall(require("nvim-navic").is_available)
    if status and is_available then
      ---@diagnostic disable-next-line
      local status, _ = pcall(vim.cmd, "TSContextDisable")
    else
      ---@diagnostic disable-next-line
      local staus, _ = pcall(vim.cmd, "TSContextEnable")
    end
  end,
})
