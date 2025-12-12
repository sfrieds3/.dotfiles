local M = {}

function M.load(colors_name, palette)
  if vim.g.colors_name then
    vim.cmd("hi clear")
  end

  vim.o.termguicolors = true
  vim.g.colors_name = colors_name

  local palette = require(palette)
  local highlights = require("ghostty-default.highlights")

  local colors = palette.colors
  local hl = highlights.setup(colors)

  for group, settings in pairs(hl) do
    vim.api.nvim_set_hl(0, group, settings)
  end

  vim.g.terminal_color_0 = colors.black
  vim.g.terminal_color_1 = colors.red
  vim.g.terminal_color_2 = colors.green
  vim.g.terminal_color_3 = colors.yellow
  vim.g.terminal_color_4 = colors.blue
  vim.g.terminal_color_5 = colors.magenta
  vim.g.terminal_color_6 = colors.cyan
  vim.g.terminal_color_7 = colors.white
  vim.g.terminal_color_8 = colors.bright_black
  vim.g.terminal_color_9 = colors.bright_red
  vim.g.terminal_color_10 = colors.bright_green
  vim.g.terminal_color_11 = colors.bright_yellow
  vim.g.terminal_color_12 = colors.bright_blue
  vim.g.terminal_color_13 = colors.bright_magenta
  vim.g.terminal_color_14 = colors.bright_cyan
  vim.g.terminal_color_15 = colors.bright_white
end

return M
