local M = {}

M.colors = {
  none = "NONE",

  bg = "#131313",
  fg = "#e8e8e8",

  black = "#3a3a3a",
  red = "#e88a8a",
  green = "#a8d8a8",
  yellow = "#e8d8a8",
  blue = "#a8c8e8",
  magenta = "#d8a8d8",
  cyan = "#a8d8d8",
  white = "#d8d8d8",

  bright_black = "#5a5a5a",
  bright_red = "#f8a8a8",
  bright_green = "#c8e8c8",
  bright_yellow = "#f8e8b8",
  bright_blue = "#b8d8f8",
  bright_magenta = "#e8b8e8",
  bright_cyan = "#b8e8e8",
  bright_white = "#f8f8f8",

  cursor_bg = "#e8e8e8",
  cursor_fg = "#131313",
  selection_bg = "#e8e8e8",
  selection_fg = "#131313",

  bg_highlight = "#282828",
  bg_visual = "#383838",
  bg_sidebar = "#1a1a1a",
  bg_float = "#1a1a1a",

  fg_gutter = "#484848",
  fg_sidebar = "#b8b8b8",

  comment = "#787878",

  diff_add = "#1a2818",
  diff_delete = "#281818",
  diff_change = "#282818",
  diff_text = "#383828",
}

M.colors.bg_dark = "#0a0a0a"
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
