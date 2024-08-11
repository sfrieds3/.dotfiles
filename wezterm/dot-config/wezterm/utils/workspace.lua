local wezterm = require("wezterm")
local act = wezterm.action
local M = {}
M.previous_workspace = "default"
M.current_workspace = "default"

wezterm.on("workspace:changed", function(window, previous_workspace, new_workspace)
  M.previous_workspace = previous_workspace or M.previous_workspace
  M.current_workspace = new_workspace or M.current_workspace

  -- window:toast_notification("Workspace Changed!", M.previous_workspace, nil, 40000)
end)

function M.switch_to_previous_workspace()
  return wezterm.action_callback(function(window, pane)
    local current_workspace = window:active_workspace()
    -- window:toast_notification("Workspace", "changing workspace to " .. M.previous_workspace)
    window:perform_action(act.SwitchToWorkspace({ name = M.previous_workspace }), pane)
    M.previous_workspace = current_workspace
    M.current_workspace = M.previous_workspace
  end)
end

return M
