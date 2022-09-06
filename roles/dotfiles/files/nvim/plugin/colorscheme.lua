-- require('github-theme').setup({
--   theme_style = 'dimmed',
--   function_style = 'italic',
--   sidebars = { 'qf', 'terminal', 'packer' },
--   dark_sidebar = true,
--   dark_float = true,
-- })

vim.g.gruvbox_material_background = "hard"
vim.g.gruvbox_material_foreground = "material"
vim.g.gruvbox_material_statusline_stype = "material"
vim.g.gruvbox_better_performance = 1

vim.g.material_style = "darker"

require("material").setup({
  contrast = {
    sidebars = true, -- Enable contrast for sidebar-like windows ( for example Nvim-Tree )
    floating_windows = true, -- Enable contrast for floating windows
    line_numbers = true, -- Enable contrast background for line numbers
    sign_column = true, -- Enable contrast background for the sign column
    cursor_line = true, -- Enable darker background for the cursor line
    non_current_windows = true, -- Enable darker background for non-current windows
    popup_menu = true, -- Enable lighter background for the popup menu
  },

  italics = {
    comments = false, -- Enable italic comments
    keywords = false, -- Enable italic keywords
    functions = false, -- Enable italic functions
    strings = false, -- Enable italic strings
    variables = false, -- Enable italic variables
  },

  contrast_filetypes = { -- Specify which filetypes get the contrasted (darker) background
    "terminal", -- Darker terminal background
    "packer", -- Darker packer background
    "qf", -- Darker qf list background
  },

  high_visibility = {
    darker = true, -- Enable higher contrast text for darker style
  },

  disable = {
    colored_cursor = true, -- Disable the colored cursor
    borders = false, -- Disable borders between verticaly split windows
    background = false, -- Prevent the theme from setting the background (NeoVim then uses your teminal background)
    term_colors = false, -- Prevent the theme from setting terminal colors
    eob_lines = false, -- Hide the end-of-buffer lines
  },

  lualine_style = "stealth", -- Lualine style ( can be 'stealth' or 'default' )

  async_loading = true, -- Load parts of the theme asyncronously for faster startup (turned on by default)

  plugins = { -- Here, you can disable(set to false) plugins that you don't use or don't want to apply the theme to
    trouble = true,
    nvim_cmp = true,
    neogit = true,
    gitsigns = true,
    git_gutter = true,
    telescope = true,
    nvim_tree = true,
    sidebar_nvim = true,
    lsp_saga = false,
    nvim_dap = false,
    nvim_navic = false,
    which_key = false,
    sneak = false,
    hop = true,
    indent_blankline = false,
    nvim_illuminate = true,
    mini = false,
  },
})

vim.cmd([[ colorscheme material ]])
