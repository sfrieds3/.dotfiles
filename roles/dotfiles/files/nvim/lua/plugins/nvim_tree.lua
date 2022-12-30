local M = {
  "nvim-tree/nvim-tree.lua",
  keys = "<Leader>\\",
  dependencies = {
    "kyazdani42/nvim-web-devicons",
  },
}

function M.config()
  require("nvim-tree").setup({})
  vim.keymap.set("n", "<Leader>\\", "<Cmd>NvimTreeToggle<CR>")
  vim.keymap.set("n", "_F", "<Cmd>NvimTreeFindFileToggle<CR>")
end

return M
