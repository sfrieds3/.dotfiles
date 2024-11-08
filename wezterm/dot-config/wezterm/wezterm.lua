local wezterm = require("wezterm")
require("utils.workspace")

local config = {}

local default_shell = "zsh"
config.default_prog = {
  string.format("/opt/homebrew/bin/%s", default_shell),
  "--login",
}
-- config.color_scheme = "Gruvbox dark, pale (base16)"
-- config.color_scheme = "kanagawabones"
-- config.color_scheme = "Kanagawa Dragon (Gogh)"
-- config.color_scheme = "OneDark (base16)"
-- config.color_scheme = "tokyonight_night"
config.color_scheme = "Catppuccin Macchiato"

config.font = wezterm.font("Berkeley Mono", { weight = "Regular" })
config.line_height = 1.1
config.font_size = 11
config.use_cap_height_to_scale_fallback_fonts = true
config.freetype_load_flags = "NO_HINTING"
config.freetype_load_target = "Normal"
config.front_end = "OpenGL"

config.automatically_reload_config = true
config.disable_default_key_bindings = true
config.native_macos_fullscreen_mode = true
config.use_fancy_tab_bar = false
config.show_new_tab_button_in_tab_bar = false
config.hide_tab_bar_if_only_one_tab = true
config.status_update_interval = 1000
config.tab_max_width = 60
config.tab_bar_at_bottom = true
config.scrollback_lines = 1000000
config.audible_bell = "Disabled"
config.window_decorations = "RESIZE"
config.window_background_opacity = 0.95
config.macos_window_background_blur = 33
config.window_close_confirmation = "AlwaysPrompt"
config.window_padding = {
  left = 0,
  right = 0,
  top = 0,
  bottom = 0,
}
config.window_frame = {
  font = wezterm.font({ family = "Berkeley Mono Variable", weight = "Bold" }),
  font_size = 13,
}

config.quick_select_patterns = {
  "([[:ascii:]].*-)[[:ascii:]].*[[:space:]]", -- kubernetes pod names
}

-- mux config
-- unix_domains = { { name = "unix" } },
-- default_gui_startup_args = { "connect", "unix" },

wezterm.on("window-config-reloaded", function(window, pane)
  window:toast_notification("wezterm", "configuration reloaded!", nil, 4000)
end)

config.keys = {
  { key = "Enter", mods = "OPT|SHIFT|CTRL", action = wezterm.action.ToggleFullScreen },
  { key = "=", mods = "CMD", action = wezterm.action.IncreaseFontSize },
  { key = "=", mods = "OPT", action = wezterm.action.IncreaseFontSize },
  { key = "-", mods = "CMD", action = wezterm.action.DecreaseFontSize },
  { key = "-", mods = "OPT", action = wezterm.action.DecreaseFontSize },
  { key = "v", mods = "CTRL|SHIFT", action = wezterm.action.PasteFrom("Clipboard") },
  { key = "v", mods = "OPT|SHIFT", action = wezterm.action.PasteFrom("Clipboard") },
}

-- require("utils.tabs").setup()
-- require("utils.status").setup()
-- require("utils.keymap").apply_to_config(config)

return config
