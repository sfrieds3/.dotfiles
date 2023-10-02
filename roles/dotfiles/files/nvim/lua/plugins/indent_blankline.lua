local M = {
  "lukas-reineke/indent-blankline.nvim",
  main = "ibl",
}

function M.config()
  require("ibl").setup({
    enabled = true,
    indent = { smart_indent_cap = true },
    scope = { show_start = true, show_end = true, highlight = { "Whitespace" } },
  })
end

return M
