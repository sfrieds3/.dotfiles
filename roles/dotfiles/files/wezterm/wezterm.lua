local wezterm = require("wezterm")
require("utils.workspace")

local config = {
  default_prog = {
    "/opt/homebrew/bin/fish",
    "--login",
  },
  color_scheme = "Gruvbox dark, pale (base16)",

  font = wezterm.font("Berkeley Mono", { weight = "Regular" }),
  line_height = 1.1,
  font_size = 11,
  use_cap_height_to_scale_fallback_fonts = true,

  automatically_reload_config = true,
  disable_default_key_bindings = true,
  native_macos_fullscreen_mode = true,
  use_fancy_tab_bar = false,
  show_new_tab_button_in_tab_bar = false,
  hide_tab_bar_if_only_one_tab = false,
  status_update_interval = 1000,
  tab_max_width = 60,
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
  -- unix_domains = { { name = "unix" } },
  -- default_gui_startup_args = { "connect", "unix" },
}

wezterm.on("window-config-reloaded", function(window, pane)
  window:toast_notification("wezterm", "configuration reloaded!", nil, 4000)
end)

require("utils.tabs").setup()
require("utils.status").setup()
require("utils.keymap").apply_to_config(config)

return config
