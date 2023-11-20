local wezterm = require("wezterm")
local config = {}

config.default_prog = {
  "/opt/homebrew/bin/fish",
  "--login",
}
config.color_scheme = "Gruvbox dark, pale (base16)"

config.enable_tab_bar = false

return config
