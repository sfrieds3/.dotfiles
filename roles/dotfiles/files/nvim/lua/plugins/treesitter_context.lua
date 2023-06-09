local M = {
  "nvim-treesitter/nvim-treesitter-context",
  event = "BufReadPre",
}

function M.config()
  require("treesitter-context").setup({
    enable = false,
  })
  vim.keymap.set("n", "\\tt", function()
    vim.cmd([[ TSContextToggle ]])
  end, { desc = "Toggle treesitter-context" })
end

return M
