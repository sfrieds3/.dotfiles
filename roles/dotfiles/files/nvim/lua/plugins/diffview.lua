local M = {
  "sindrets/diffview.nvim",
  dependencies = "nvim-lua/plenary.nvim",
  cmd = {
    "DiffviewOpen",
    "DiffviewClose",
    "DiffviewToggleFiles",
    "DiffviewFocusFiles",
    "DiffViewLog",
    "DiffViewFileHistory",
  },
  keys = { "<Leader>gd", "<Leader>gh", "<Leader>gl" },
}

function M.config()
  require("diffview").setup({})

  vim.keymap.set("n", "<Leader>gd", "<Cmd>DiffviewOpen<CR>", { desc = "Diffview: [g]oto [d]iff" })
  vim.keymap.set("n", "<Leader>gh", "<Cmd>DiffviewFileHistory<CR>", { desc = "Diffview: [g]oto file [h] history" })

  vim.api.nvim_create_autocmd({ "FileType" }, {
    group = vim.api.nvim_create_augroup("DiffViewEnter", { clear = true }),
    pattern = { "DiffViewFiles", "DiffviewFileHistory" },
    callback = function(event)
      vim.bo[event.buf].buflisted = false
      vim.keymap.set("n", "q", "<Cmd>DiffviewClose<CR>", { buffer = event.buf, silent = true })
    end,
  })
end

return M
