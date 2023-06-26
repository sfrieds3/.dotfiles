local M = {
  "s1n7ax/nvim-window-picker",
  version = "v2.*",
}

function M.config()
  require("window-picker").setup({
    hint = "floating-big-letter",
  })

  vim.keymap.set("n", "<Leader>w", function()
    local winid = require("window-picker").pick_window() or vim.api.nvim_get_current_win()
    vim.api.nvim_set_current_win(winid)
  end, { desc = "Pick a window" })
end

return M
