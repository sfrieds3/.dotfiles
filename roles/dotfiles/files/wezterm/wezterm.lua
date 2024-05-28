local wezterm = require("wezterm")
local act = wezterm.action

local config = {
  default_prog = {
    "/opt/homebrew/bin/fish",
    "--login",
  },
  color_scheme = "Gruvbox dark, pale (base16)",

  font = wezterm.font_with_fallback({
    "Monaspace Neon",
    "Symbols Nerd Font Mono",
  }),
  line_height = 1.1,
  font_size = 11,
  use_cap_height_to_scale_fallback_fonts = true,
  harfbuzz_features = { "calt", "liga", "ss01", "ss02", "ss03", "ss04", "ss05", "ss06", "ss07", "ss08" },
  font_rules = {
    {
      intensity = "Normal",
      italic = true,
      font = wezterm.font("Monaspace Radon", { weight = "Regular" }),
    },
    {
      intensity = "Bold",
      italic = false,
      font = wezterm.font("Monaspace Neon", { weight = "ExtraBold" }),
    },
    {
      intensity = "Bold",
      italic = true,
      font = wezterm.font("Monaspace Radon", { weight = "ExtraBold" }),
    },
  },

  disable_default_key_bindings = true,
  native_macos_fullscreen_mode = true,
  enable_tab_bar = true,
  tab_bar_at_bottom = true,
  scrollback_lines = 1000000,
  audible_bell = "Disabled",
  window_decorations = "RESIZE",
  window_close_confirmation = "AlwaysPrompt",
  window_padding = {
    left = 0,
    right = 0,
    top = 0,
    bottom = 0,
  },

  -- mux config
  unix_domains = { { name = "unix" } },
  default_gui_startup_args = { "connect", "unix" },
}

require("utils.tabs").setup()
require("utils.status").setup()
require("utils.keymap").apply_to_config(config)

return config
