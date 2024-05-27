---@diagnostic disable: undefined-field
local wezterm = require("wezterm") ---@class WezTerm

local M = {}

function M.setup()
  wezterm.on("update-right-status", function(window, pane)
    local cells = {}

    local cwd_uri = pane:get_current_working_dir()
    if cwd_uri then
      local cwd = ""
      local hostname = ""

      if type(cwd_uri) == "userdata" then
        cwd = cwd_uri.file_path
        hostname = cwd_uri.host or wezterm.hostname()
      else
        cwd_uri = cwd_uri:sub(8)
        local slash = cwd_uri:find("/")
        if slash then
          hostname = cwd_uri:sub(1, slash - 1)
          cwd = cwd_uri:sub(slash):gsub("%%(%x%x)", function(hex)
            return string.char(tonumber(hex, 16))
          end)
        end
      end

      local dot = hostname:find("[.]")
      if dot then
        hostname = hostname:sub(1, dot - 1)
      end
      if hostname == "" then
        hostname = wezterm.hostname()
      end

      table.insert(cells, cwd)
      table.insert(cells, hostname)
    end

    local date = wezterm.strftime("%a %-d %b - %H:%M:%S")
    table.insert(cells, date)

    for _, b in ipairs(wezterm.battery_info()) do
      table.insert(cells, string.format("%.0f%%", b.state_of_charge * 100))
    end

    local statusline_colors = {
      "Black",
      "Black",
      "Black",
      "Black",
    }

    local text_fg = "White"

    local elements = {}
    local num_cells = 0

    local function push(text, is_last)
      local cell_no = num_cells + 1
      table.insert(elements, { Foreground = { Color = text_fg } })
      table.insert(elements, { Background = { Color = statusline_colors[cell_no] } })
      table.insert(elements, { Text = " " .. text .. " " })
      if not is_last then
        table.insert(elements, { Foreground = { Color = statusline_colors[cell_no + 1] } })
      end
      num_cells = num_cells + 1
    end

    while #cells > 0 do
      local cell = table.remove(cells, 1)
      push(cell, #cells == 0)
    end

    window:set_right_status(wezterm.format(elements))
  end)
end

return M
