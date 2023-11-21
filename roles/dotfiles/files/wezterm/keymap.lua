local wezterm = require("wezterm")
local act = wezterm.action
local keymap = {}

function keymap.apply_to_config(config)
  config.leader = { key = "e", mods = "OPT", timeout_milliseconds = 1000 }
  config.keys = {
    -- font size
    { key = "=", mods = "CMD", action = wezterm.action.IncreaseFontSize },
    { key = "=", mods = "OPT", action = wezterm.action.IncreaseFontSize },
    { key = "-", mods = "CMD", action = wezterm.action.DecreaseFontSize },
    { key = "-", mods = "OPT", action = wezterm.action.DecreaseFontSize },
    -- splits
    {
      key = "%",
      mods = "LEADER|SHIFT",
      action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }),
    },
    {
      key = '"',
      mods = "LEADER|SHIFT",
      action = act.SplitVertical({ domain = "CurrentPaneDomain" }),
    },

    -- workspaces
    {
      key = "q",
      mods = "OPT",
      action = act.ShowLauncherArgs({ flags = "FUZZY|WORKSPACES" }),
    },
    { key = "n", mods = "OPT|SHIFT", action = act.SwitchWorkspaceRelative(1) },
    { key = "p", mods = "OPT|SHIFT", action = act.SwitchWorkspaceRelative(-1) },

    { key = "n", mods = "OPT", action = act.ActivateTabRelative(1) },
    { key = "p", mods = "OPT", action = act.ActivateTabRelative(-1) },
    {
      key = "o",
      mods = "OPT",
      action = wezterm.action.ActivateLastTab,
    },
    {
      key = "c",
      mods = "LEADER",
      action = act.SpawnTab("CurrentPaneDomain"),
    },
    -- { key = "$", mods = "LEADER|SHIFT", action = wezterm.rename_word

    -- select pane
    {
      key = "w",
      mods = "LEADER",
      action = act.PaneSelect({
        alphabet = "jkl;hunim",
      }),
    },
    {
      key = "h",
      mods = "LEADER",
      action = act.ActivatePaneDirection("Left"),
    },
    {
      key = "l",
      mods = "LEADER",
      action = act.ActivatePaneDirection("Right"),
    },
    {
      key = "k",
      mods = "LEADER",
      action = act.ActivatePaneDirection("Up"),
    },
    {
      key = "j",
      mods = "LEADER",
      action = act.ActivatePaneDirection("Down"),
    },
  }
end

return keymap
