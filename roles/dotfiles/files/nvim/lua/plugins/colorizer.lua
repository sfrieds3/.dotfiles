local M = {
  "norcalli/nvim-colorizer.lua",
  ft = { "html", "css", "javascript", "vim", "eruby" },
  cmd = { "ColorizerToggle", "ColorizerAttachToBuffer" },
}

function M.config()
  require("colorizer").setup({
    default_options = {
      RGB = false,
    },
  })

  vim.keymap.set("n", "_C", "<Cmd>ColorizerToggle<CR>")
end

return M
