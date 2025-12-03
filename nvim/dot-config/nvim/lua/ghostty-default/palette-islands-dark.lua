local M = {}

M.colors = {
  none = "NONE",

  bg = "#1a1a1a",
  fg = "#dddddd",

  black = "#111111",
  red = "#bb6666",
  green = "#88aa77",
  yellow = "#ccaa66",
  blue = "#7799bb",
  magenta = "#aa88aa",
  cyan = "#77aaa0",
  white = "#bbbbbb",

  bright_black = "#555555",
  bright_red = "#cc7777",
  bright_green = "#99bb88",
  bright_yellow = "#ddbb77",
  bright_blue = "#88aacc",
  bright_magenta = "#bb99bb",
  bright_cyan = "#88bbaa",
  bright_white = "#dddddd",

  cursor_bg = "#dddddd",
  cursor_fg = "#1a1a1a",
  selection_bg = "#334455",
  selection_fg = "#dddddd",

  bg_highlight = "#252525",
  bg_visual = "#2a3344",
  bg_sidebar = "#151515",
  bg_float = "#252525",

  fg_gutter = "#444444",
  fg_sidebar = "#999999",

  comment = "#666666",

  diff_add = "#1a2a1a",
  diff_delete = "#2a1a1a",
  diff_change = "#2a2a1a",
  diff_text = "#3a3a2a",
}

M.colors.bg_dark = M.colors.black
M.colors.bg_statusline = M.colors.bg_sidebar
M.colors.bg_popup = M.colors.bg_float

M.colors.fg_dark = M.colors.white

M.colors.border = "#555555"
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
