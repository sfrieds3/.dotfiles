local M = {
  "akinsho/bufferline.nvim",
  version = "*",
}

function M.config()
  require("bufferline").setup({
    options = {
      diagnostics = "nvim_lsp",
      mode = "tabs",
    },
  })
end

return M
