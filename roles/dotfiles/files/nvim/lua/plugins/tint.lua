local M = {
  "levouh/tint.nvim",
}

function M.config()
  require("tint").setup({
    tint = -33,
    saturation = 0.6,
    window_ignore_function = function(winid)
      local exclude_filetypes = {
        ["neo-tree"] = true,
      }
      local bufid = vim.api.nvim_win_get_buf(winid)
      local buftype = vim.api.nvim_get_option_value("buftype", { buf = bufid })
      local floating = vim.api.nvim_win_get_config(winid).relative ~= ""
      local filetype = vim.api.nvim_get_option_value("filetype", { buf = bufid })

      -- Do not tint `terminal` or floating windows, tint everything else
      return buftype == "terminal" or floating or exclude_filetypes[filetype]
    end,
  })
end

return M
