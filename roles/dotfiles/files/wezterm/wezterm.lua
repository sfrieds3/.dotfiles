local w = require("wezterm")
local keymap = require("keymap")
require("status")

local config = {
  default_prog = {
    "/opt/homebrew/bin/fish",
    "--login",
  },
  color_scheme = "Gruvbox dark, pale (base16)",
  font = w.font("Hack Nerd Font"),
  font_size = 11,
  disable_default_key_bindings = true,
  enable_tab_bar = true,
  tab_bar_at_bottom = true,
  scrollback_lines = 1000000,
  window_padding = {
    left = 0,
    right = 0,
    top = 0,
    bottom = 0,
  },
}

keymap.apply_to_config(config)

return config
