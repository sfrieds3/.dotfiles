local wezterm = require("wezterm")
local act = wezterm.action
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
          win:perform_action({ AdjustPaneSize = { direction_keys[key], 3 } }, pane)
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
    -- window management
    { key = "Enter", mods = "OPT|SHIFT|CTRL", action = act.ToggleFullScreen },

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

    -- font swticher
    { key = "F", mods = "LEADER", action = require("utils.font").selector_action() },

    -- theme switcher
    {
      key = "t",
      mods = "OPT|SHIFT|CTRL",
      action = wezterm.action_callback(function(window, pane)
        require("theme_switcher").theme_switcher(window, pane)
      end),
    },

    -- scrollback
    { key = "k", mods = "OPT|SHIFT|CTRL", action = act.ScrollByPage(-1) },
    { key = "j", mods = "OPT|SHIFT|CTRL", action = act.ScrollByPage(1) },

    -- workspaces
    {
      key = "q",
      mods = "OPT|SHIFT",
      action = act.ShowLauncherArgs({ flags = "FUZZY|WORKSPACES" }),
    },

    {
      key = "q",
      mods = "OPT",
      action = require("utils.workspace_switcher").switch_workspace(),
    },
    { key = "n", mods = "OPT|SHIFT", action = act.SwitchWorkspaceRelative(1) },
    { key = "p", mods = "OPT|SHIFT", action = act.SwitchWorkspaceRelative(-1) },
    { key = "c", mods = "LEADER|OPT", action = act.SwitchToWorkspace },
    {
      key = "o",
      mods = "OPT|SHIFT",
      action = require("utils.workspace").switch_to_previous_workspace(),
    },

    -- Prompt for a name to use for a new workspace and switch to it.
    {
      key = "c",
      mods = "LEADER|OPT|SHIFT",
      action = act.PromptInputLine({
        description = wezterm.format({
          { Attribute = { Intensity = "Bold" } },
          { Foreground = { AnsiColor = "Fuchsia" } },
          { Text = "Enter name for new workspace" },
        }),
        action = wezterm.action_callback(function(window, pane, line)
          if line then
            window:perform_action(
              act.SwitchToWorkspace({
                name = line,
              }),
              pane
            )
          end
        end),
      }),
    },

    { key = "n", mods = "OPT", action = act.ActivateTabRelative(1) },
    { key = "p", mods = "OPT", action = act.ActivateTabRelative(-1) },
    {
      key = "o",
      mods = "OPT",
      action = act.ActivateLastTab,
    },
    { key = "F9", mods = "OPT", action = wezterm.action.ShowTabNavigator },
    {
      key = "c",
      mods = "LEADER",
      action = act.SpawnTab("CurrentPaneDomain"),
    },

    {
      key = "$",
      mods = "LEADER",
      action = act.PromptInputLine({
        description = "Enter new name for workspace",
        action = wezterm.action_callback(function(window, pane, line)
          if line then
            wezterm.mux.rename_workspace(wezterm.mux.get_active_workspace(), line)
          end
        end),
      }),
    },

    {
      key = ",",
      mods = "LEADER",
      action = act.PromptInputLine({
        description = "Enter new name for tab",
        action = wezterm.action_callback(function(window, pane, line)
          if line then
            window:active_tab():set_title(line)
          end
        end),
      }),
    },

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
    {
      key = "x",
      mods = "OPT|SHIFT",
      action = wezterm.action.CloseCurrentPane({ confirm = true }),
    },
    {
      key = "x",
      mods = "OPT",
      action = wezterm.action.CloseCurrentTab({ confirm = true }),
    },

    -- panes
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

    -- resize panes
    -- move between split panes
    split_nav("move", "OPT", "h"),
    split_nav("move", "OPT", "j"),
    split_nav("move", "OPT", "k"),
    split_nav("move", "OPT", "l"),
    -- resize panes
    split_nav("resize", "OPT|SHIFT", "h"),
    split_nav("resize", "OPT|SHIFT", "j"),
    split_nav("resize", "OPT|SHIFT", "k"),
    split_nav("resize", "OPT|SHIFT", "l"),

    -- debug
    { key = "L", mods = "CTRL", action = wezterm.action.ShowDebugOverlay },

    -- search for things that look like git hashes
    {
      key = "/",
      mods = "LEADER",
      action = act.Search("CurrentSelectionOrEmptyString"),
    },
    {
      key = "?",
      mods = "LEADER",
      action = act.Search("CurrentSelectionOrEmptyString"),
    },
    { key = " ", mods = "LEADER", action = act.QuickSelect },
    {
      key = "H",
      mods = "SHIFT|CTRL",
      action = act.Search({ Regex = "[a-f0-9]{6,}" }),
    },
    { key = "1", mods = "OPT", action = act.ActivateTab(0) },
    { key = "2", mods = "OPT", action = act.ActivateTab(1) },
    { key = "3", mods = "OPT", action = act.ActivateTab(2) },
    { key = "4", mods = "OPT", action = act.ActivateTab(3) },
    { key = "5", mods = "OPT", action = act.ActivateTab(4) },
    { key = "6", mods = "OPT", action = act.ActivateTab(5) },
    { key = "7", mods = "OPT", action = act.ActivateTab(6) },
    { key = "8", mods = "OPT", action = act.ActivateTab(7) },
    { key = "9", mods = "OPT", action = act.ActivateTab(-1) },
  }
  config.key_tables = {
    copy_mode = {
      { key = "Escape", mods = "NONE", action = act.CopyMode("Close") },
      { key = "h", mods = "NONE", action = act.CopyMode("MoveLeft") },
      { key = "j", mods = "NONE", action = act.CopyMode("MoveDown") },
      { key = "k", mods = "NONE", action = act.CopyMode("MoveUp") },
      { key = "l", mods = "NONE", action = act.CopyMode("MoveRight") },

      { key = "/", mods = "NONE", action = wezterm.action({ Search = { CaseSensitiveString = "" } }) },

      { key = "n", mods = "NONE", action = act.CopyMode("NextMatch") },
      { key = "n", mods = "SHIFT", action = act.CopyMode("PriorMatch") },
      { key = "y", mods = "NONE", action = act.CopyTo("ClipboardAndPrimarySelection") },
    },
    search_mode = {
      { key = "Escape", mods = "NONE", action = wezterm.action({ CopyMode = "Close" }) },
      -- Go back to copy mode when pressing enter, so that we can use unmodified keys like "n"
      -- to navigate search results without conflicting with typing into the search area.
      { key = "Tab", mods = "NONE", action = act.CopyMode("NextMatch") },
      { key = "Tab", mods = "SHIFT", action = act.CopyMode("PriorMatch") },
      { key = "n", mods = "CTRL", action = act.CopyMode("NextMatch") },
      { key = "p", mods = "CTRL", action = act.CopyMode("PriorMatch") },
      { key = "r", mods = "CTRL", action = act.CopyMode("CycleMatchType") },
      { key = "w", mods = "CTRL", action = act.CopyMode("ClearPattern") },
      { key = "Enter", mods = "NONE", action = "ActivateCopyMode" },
    },
  }
end

return keymap
