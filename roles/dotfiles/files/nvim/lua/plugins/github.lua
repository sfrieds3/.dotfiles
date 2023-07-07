local M = {
  "projekt0n/github-nvim-theme",
  version = "*",
}

function M.config()
  require("github-theme").setup({
    options = {
      dim_inactive = true,
      styles = {
        functions = "italic",
      },
      inverse = { -- Inverse highlight for different types
        match_paren = true,
        visual = true,
        search = true,
      },
      darken = {
        floats = true,
        sidebars = {
          enable = true,
          list = { "qf", "terminal" },
        },
      },
    },
  })
end

return M
