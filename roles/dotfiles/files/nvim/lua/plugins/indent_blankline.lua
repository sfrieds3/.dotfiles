local M = {
  "lukas-reineke/indent-blankline.nvim",
}

function M.config()
  require("indent_blankline").setup({
    space_char_blankline = " ",
    show_current_context = true,
    show_current_context_start = false,
  })
end

return M