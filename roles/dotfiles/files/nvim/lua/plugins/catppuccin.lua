local M = {
  "catppuccin/nvim",
  name = "catppuccin",
  lazy = true,
}

function M.config()
  require("catppuccin").setup({
    flavour = "mocha", -- latte, frappe, macchiato, mocha
    background = {
      light = "frappe",
      dark = "mocha",
    },
    term_colors = true,
    dim_inactive = {
      enabled = true,
      shade = "dark",
      percentage = 0.15,
    },
    transparent_background = false,
    no_italic = false,
    no_bold = false,
    styles = {
      comments = { "italic" },
      conditionals = {},
      loops = {},
      functions = {},
      keywords = {},
      strings = {},
      variables = {},
      numbers = {},
      booleans = { "bold" },
      properties = {},
      types = {},
    },
    integrations = {
      cmp = true,
      dap = true,
      gitsigns = true,
      mason = true,
      navic = true,
      neogit = true,
      neotree = true,
      nvimtree = true,
      symbols_outline = true,
      telescope = true,
      treesitter = true,
      treesitter_context = true,
      lsp_trouble = true,
    },
    color_overrides = {
      mocha = {
        base = "#131313",
        mantle = "#131313",
        crust = "#131313",
      },
    },
  })
end

return M
