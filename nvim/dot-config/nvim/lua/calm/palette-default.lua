local M = {}

M.colors = {
  none = "NONE",

  bg = "#282c34",
  fg = "#ffffff",

  black = "#3a3f4b", -- real: #1d1f21
  red = "#cc6566",
  green = "#b6bd68",
  yellow = "#f0c674",
  blue = "#82a2be",
  magenta = "#b294bb",
  cyan = "#8abeb7",
  white = "#c4c8c6",

  bright_black = "#666666",
  bright_red = "#d54e53",
  bright_green = "#b9ca4b",
  bright_yellow = "#e7c547",
  bright_blue = "#7aa6da",
  bright_magenta = "#c397d8",
  bright_cyan = "#70c0b1",
  bright_white = "#eaeaea",

  cursor_bg = "#ffffff",
  cursor_fg = "#353a44",
  selection_bg = "#ffffff",
  selection_fg = "#282c34",

  bg_highlight = "#353a44",
  bg_visual = "#3e4451",
  bg_sidebar = "#23272e",
  bg_float = "#353a44",

  fg_gutter = "#4b5263",
  fg_sidebar = "#abb2bf",

  comment = "#8a8a8a",

  diff_add = "#2d3a2f",
  diff_delete = "#3d2e2e",
  diff_change = "#3a3a2f",
  diff_text = "#4a4a3f",
}

M.colors.bg_dark = M.colors.black
M.colors.bg_float = M.colors.bg
M.colors.bg_statusline = M.colors.bg_sidebar
M.colors.bg_popup = M.colors.bg_sidebar

M.colors.fg_dark = M.colors.white

M.colors.border = M.colors.fg_gutter
M.colors.nontext = M.colors.bg_visual

M.colors.git_add = M.colors.green
M.colors.git_change = M.colors.yellow
M.colors.git_delete = M.colors.red
M.colors.git_text = M.colors.blue

M.colors.error = M.colors.bright_red
M.colors.warning = M.colors.bright_yellow
M.colors.info = M.colors.bright_blue
M.colors.hint = M.colors.bright_cyan

return M
