local wezterm = require("wezterm")
local keymap = require("keymap")
require("status")

local config = {
  default_prog = {
    "/opt/homebrew/bin/fish",
    "--login",
  },
  color_scheme = "Gruvbox dark, pale (base16)",
  font = wezterm.font_with_fallback({
    "Hack Nerd Font",
    "Symbols Nerd Font Mono",
  }),
  line_height = 1.1,
  font_size = 12,
  use_cap_height_to_scale_fallback_fonts = true,
  disable_default_key_bindings = false,
  enable_tab_bar = true,
  tab_bar_at_bottom = true,
  scrollback_lines = 1000000,
  window_decorations = "RESIZE",
  window_close_confirmation = "AlwaysPrompt",
  window_padding = {
    left = 0,
    right = 0,
    top = 0,
    bottom = 0,
  },
}

keymap.apply_to_config(config)

return config
