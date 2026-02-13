local M = {}

M.colors = {
  none = "NONE",

  bg = "#131313",
  fg = "#ffffff",

  black = "#111111",
  red = "#cc6666",
  green = "#99bb66",
  yellow = "#ddcc66",
  blue = "#88aacc",
  magenta = "#bb99bb",
  cyan = "#88bbaa",
  white = "#cccccc",

  bright_black = "#666666",
  bright_red = "#dd5555",
  bright_green = "#bbcc55",
  bright_yellow = "#eecc55",
  bright_blue = "#77aadd",
  bright_magenta = "#cc99dd",
  bright_cyan = "#77ccbb",
  bright_white = "#eeeeee",

  cursor_bg = "#ffffff",
  cursor_fg = "#333333",
  selection_bg = "#ffffff",
  selection_fg = "#222222",

  bg_highlight = "#333333",
  bg_visual = "#444444",
  bg_sidebar = "#1a1a1a",
  bg_float = "#333333",

  fg_gutter = "#555555",
  fg_sidebar = "#aabbcc",

  comment = "#888888",

  diff_add = "#223322",
  diff_delete = "#332222",
  diff_change = "#333322",
  diff_text = "#444433",
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
