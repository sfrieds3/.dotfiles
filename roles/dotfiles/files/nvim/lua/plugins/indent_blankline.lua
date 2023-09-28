local M = {
  "lukas-reineke/indent-blankline.nvim",
  main = "ibl",
}

function M.config()
  require("ibl").setup({
    space_char_blankline = " ",
    show_current_context = true,
    show_current_context_start = false,
    smart_indent_cap = true,
    scope = { highlight = { "Whitespace" } },
  })
end

return M
