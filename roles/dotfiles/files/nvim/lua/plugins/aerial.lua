local M = { "stevearc/aerial.nvim" }

function M.config()
  require("aerial").setup({
    on_attach = function(bufnr)
      -- Jump forwards/backwards with '{' and '}'
      vim.keymap.set("n", "{", "<cmd>AerialPrev<CR>", { buffer = bufnr })
      vim.keymap.set("n", "}", "<cmd>AerialNext<CR>", { buffer = bufnr })
    end,
    layout = {
      min_width = { 20, 0.25 },
      max_width = { 40, 0.25 },
      default_direction = "prefer_right",
      placement = "edge",
    },
    attach_mode = "window",
  })

  vim.keymap.set("n", "<Leader>A", "<Cmd>AerialToggle<CR>")
end

return M
