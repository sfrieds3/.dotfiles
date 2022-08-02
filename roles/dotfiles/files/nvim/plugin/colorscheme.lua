-- require('github-theme').setup({
--   theme_style = 'dark_default',
--   function_style = 'italic',
--   sidebars = { 'qf', 'terminal', 'packer' },
--   dark_sidebar = true,
--   dark_float = true,
-- })

require('doom-one').setup({
  terminal_colors = false,
  italic_comments = false,
  enable_treesitter = true,
  plugins_integrations = {
    neorg = false,
    barbar = false,
    bufferline = false,
    gitsigns = true,
    telescope = true,
    neogit = true,
    nvim_tree = false,
    dashboard = false,
    startify = false,
    whichkey = false,
    indent_blankline = false,
    vim_illuminate = true,
    lspsaga = false,
  },
})
