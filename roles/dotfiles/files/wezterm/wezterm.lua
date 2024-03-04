local wezterm = require("wezterm")
local act = wezterm.action
require("tabs")

local config = {
  default_prog = {
    "/bin/zsh",
    "--login",
  },
  color_scheme = "Gruvbox dark, pale (base16)",
  font = wezterm.font_with_fallback({
    "JetBrains Mono",
    "Symbols Nerd Font Mono",
  }),
  line_height = 1.1,
  font_size = 11,
  use_cap_height_to_scale_fallback_fonts = true,
  disable_default_key_bindings = false,
  native_macos_fullscreen_mode = true,
  enable_tab_bar = false,
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
  -- stylua: ignore
  keys = {
    -- copy/paste
    { key = "c", mods = "CTRL|SHIFT", action = act.CopyTo("ClipboardAndPrimarySelection") },
    { key = "c", mods = "OPT|SHIFT", action = act.CopyTo("ClipboardAndPrimarySelection") },
    { key = "c", mods = "OPT|SHIFT|CTRL", action = act.CopyTo("PrimarySelection") },
    -- paste from the clipboard
    { key = "v", mods = "CTRL|SHIFT", action = act.PasteFrom("Clipboard") },
    { key = "v", mods = "OPT|SHIFT", action = act.PasteFrom("Clipboard") },

    -- paste from the primary selection
    { key = "v", mods = "OPT|SHIFT|CTRL", action = act.PasteFrom("PrimarySelection") },

    -- font size
    { key = "=", mods = "CMD", action = act.IncreaseFontSize },
    { key = "=", mods = "OPT", action = act.IncreaseFontSize },
    { key = "-", mods = "CMD", action = act.DecreaseFontSize },
    { key = "-", mods = "OPT", action = act.DecreaseFontSize },
  },
}

-- keymap.apply_to_config(config)

return config
