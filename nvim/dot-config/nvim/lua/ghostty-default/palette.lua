local M = {}

M.colors = {
  none = "NONE",

  bg = "#282c34",
  fg = "#ffffff",

  black = "#1d1f21",
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
}

M.colors.bg_dark = "#1d1f21"
M.colors.bg_highlight = "#353a44"
M.colors.bg_visual = "#3e4451"
M.colors.bg_float = "#282c34"
M.colors.bg_sidebar = "#23272e"
M.colors.bg_statusline = "#23272e"
M.colors.bg_popup = "#23272e"

M.colors.fg_dark = "#c4c8c6"
M.colors.fg_gutter = "#4b5263"
M.colors.fg_sidebar = "#abb2bf"

M.colors.border = "#4b5263"
M.colors.comment = "#8a8a8a"
M.colors.nontext = "#3e4451"

M.colors.git_add = "#b6bd68"
M.colors.git_change = "#f0c674"
M.colors.git_delete = "#cc6566"
M.colors.git_text = "#82a2be"

M.colors.error = "#d54e53"
M.colors.warning = "#e7c547"
M.colors.info = "#7aa6da"
M.colors.hint = "#70c0b1"

M.colors.diff_add = "#2d3a2f"
M.colors.diff_delete = "#3d2e2e"
M.colors.diff_change = "#3a3a2f"
M.colors.diff_text = "#4a4a3f"

return M
