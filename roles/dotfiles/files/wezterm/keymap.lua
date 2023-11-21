local wezterm = require("wezterm")
local keymap = {}

-- nvim smart-split integration
local function is_vim(pane)
  -- this is set by the plugin, and unset on ExitPre in Neovim
  return pane:get_user_vars().IS_NVIM == "true"
end

local direction_keys = {
  Left = "h",
  Down = "j",
  Up = "k",
  Right = "l",
  -- reverse lookup
  h = "Left",
  j = "Down",
  k = "Up",
  l = "Right",
}

local function split_nav(resize_or_move, mods, key)
  return {
    key = key,
    mods = mods,
    action = wezterm.action_callback(function(win, pane)
      if is_vim(pane) then
        -- pass the keys through to vim/nvim
        win:perform_action({
          SendKey = { key = key, mods = mods },
        }, pane)
      else
        if resize_or_move == "resize" then
          win:perform_action({ AdjustPaneSize = { key, 3 } }, pane)
        else
          win:perform_action({ ActivatePaneDirection = direction_keys[key] }, pane)
        end
      end
    end),
  }
end

function keymap.apply_to_config(config)
  config.leader = { key = "e", mods = "OPT", timeout_milliseconds = 1000 }
  config.keys = {
    -- font size
    { key = "=", mods = "CMD", action = wezterm.action.IncreaseFontSize },
    { key = "=", mods = "OPT", action = wezterm.action.IncreaseFontSize },
    { key = "-", mods = "CMD", action = wezterm.action.DecreaseFontSize },
    { key = "-", mods = "OPT", action = wezterm.action.DecreaseFontSize },

    -- workspaces
    {
      key = "q",
      mods = "OPT",
      action = wezterm.action.ShowLauncherArgs({ flags = "FUZZY|WORKSPACES" }),
    },
    { key = "n", mods = "OPT|SHIFT", action = wezterm.action.SwitchWorkspaceRelative(1) },
    { key = "p", mods = "OPT|SHIFT", action = wezterm.action.SwitchWorkspaceRelative(-1) },

    { key = "n", mods = "OPT", action = wezterm.action.ActivateTabRelative(1) },
    { key = "p", mods = "OPT", action = wezterm.action.ActivateTabRelative(-1) },
    {
      key = "o",
      mods = "OPT",
      action = wezterm.action.ActivateLastTab,
    },
    {
      key = "c",
      mods = "LEADER",
      action = wezterm.action.SpawnTab("CurrentPaneDomain"),
    },
    -- { key = "$", mods = "LEADER|SHIFT", action = wezterm.rename_word

    -- splits
    {
      key = "%",
      mods = "LEADER|SHIFT",
      action = wezterm.action.SplitHorizontal({ domain = "CurrentPaneDomain" }),
    },
    {
      key = '"',
      mods = "LEADER|SHIFT",
      action = wezterm.action.SplitVertical({ domain = "CurrentPaneDomain" }),
    },

    -- panes
    {
      key = "w",
      mods = "LEADER",
      action = wezterm.action.PaneSelect({
        alphabet = "jkl;hunim",
      }),
    },
    {
      key = "h",
      mods = "LEADER",
      action = wezterm.action.ActivatePaneDirection("Left"),
    },
    {
      key = "l",
      mods = "LEADER",
      action = wezterm.action.ActivatePaneDirection("Right"),
    },
    {
      key = "k",
      mods = "LEADER",
      action = wezterm.action.ActivatePaneDirection("Up"),
    },
    {
      key = "j",
      mods = "LEADER",
      action = wezterm.action.ActivatePaneDirection("Down"),
    },

    -- resize panes
    -- move between split panes
    split_nav("move", "OPT", "h"),
    split_nav("move", "OPT", "j"),
    split_nav("move", "OPT", "k"),
    split_nav("move", "OPT", "l"),
    -- resize panes
    split_nav("resize", "OPT", "LeftArrow"),
    split_nav("resize", "OPT", "DownArrow"),
    split_nav("resize", "OPT", "UpArrow"),
    split_nav("resize", "OPT", "RightArrow"),

    -- find
    {
      key = "f",
      mods = "CTRL|SHIFT",
      action = wezterm.action.Find,
    },

    -- search for things that look like git hashes
    {
      key = "H",
      mods = "SHIFT|CTRL",
      action = wezterm.action.Search({ Regex = "[a-f0-9]{6,}" }),
    },
  }
  config.key_tables = {
    search_mode = {
      { key = "Enter", mods = "NONE", action = act.CopyMode("PriorMatch") },
      { key = "Escape", mods = "NONE", action = act.CopyMode("Close") },
      { key = "n", mods = "CTRL", action = act.CopyMode("NextMatch") },
      { key = "p", mods = "CTRL", action = act.CopyMode("PriorMatch") },
      { key = "r", mods = "CTRL", action = act.CopyMode("CycleMatchType") },
      { key = "u", mods = "CTRL", action = act.CopyMode("ClearPattern") },
      {
        key = "PageUp",
        mods = "NONE",
        action = act.CopyMode("PriorMatchPage"),
      },
      {
        key = "PageDown",
        mods = "NONE",
        action = act.CopyMode("NextMatchPage"),
      },
      { key = "UpArrow", mods = "NONE", action = act.CopyMode("PriorMatch") },
      { key = "DownArrow", mods = "NONE", action = act.CopyMode("NextMatch") },
    },
  }
end

return keymap
