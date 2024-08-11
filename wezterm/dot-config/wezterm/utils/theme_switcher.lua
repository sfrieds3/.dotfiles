local wezterm = require("wezterm")
local act = wezterm.action

local ThemeSwitcher = {}

ThemeSwitcher.theme_switcher = function(extra_args)
  -- get builting color schemes
  local schemes = wezterm.get_builtin_color_schemes()
  local choices = {}
  local config_path = "$XDG_CONFIG_HOME/wezterm/wezterm.lua"

  -- populate theme names in choices list
  for key, _ in pairs(schemes) do
    table.insert(choices, { id = key, label = tostring(key) })
  end

  -- sort choices list
  table.sort(choices, function(c1, c2)
    return c1.label < c2.label
  end)

  return wezterm.action_callback(function(window, pane)
    window:perform_action(
      act.InputSelector({
        title = "🎨 Pick a Theme!",
        choices = choices,
        fuzzy = true,

        -- execute 'sed' shell command to replace the line
        -- responsible of colorscheme in my config
        action = wezterm.action_callback(function(inner_window, inner_pane, id, label)
          local cmd = 's/^config.color_scheme = .*/config.color_scheme = "' .. label .. '"/'
          window:toast_notification("wezterm", "Setting theme to: " .. cmd .. " in: " .. config_path, nil, 1000)
          -- inner_window:perform_action(
          --   act.SpawnCommandInNewTab({
          --     args = {
          --       "sed",
          --       "-i",
          --       cmd,
          --       config_path,
          --     },
          --   }),
          -- inner_pane
          -- )
        end),
      }),
      pane
    )
  end)
end

return ThemeSwitcher
