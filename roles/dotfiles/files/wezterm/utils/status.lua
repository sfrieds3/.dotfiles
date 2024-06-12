---@diagnostic disable: undefined-field
local wezterm = require("wezterm") ---@class WezTerm

local M = {}

function M.setup()
  wezterm.on("update-status", function(window, pane)
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
    end

    local date = wezterm.strftime("%a %-d %b - %H:%M:%S")
    table.insert(cells, date)

    for _, b in ipairs(wezterm.battery_info()) do
      local nf = wezterm.nerdfonts
      local charge = b.state_of_charge * 100

      local function get_icon(charge_pct)
        if charge_pct < 15 then
          return nf.fa_battery_empty
        elseif charge_pct < 40 then
          return nf.fa_battery_quarter
        elseif charge_pct < 65 then
          return nf.fa_battery_half
        elseif charge_pct < 90 then
          return nf.fa_battery_three_quarters
        else
          return nf.fa_battery_full
        end
      end

      local icon = get_icon(charge)

      table.insert(cells, string.format("%.0f%% %s ", charge, icon))
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
      local sep = wezterm.nerdfonts.ple_right_half_circle_thin
      local cell_no = num_cells + 1
      table.insert(elements, { Foreground = { AnsiColor = text_fg } })
      table.insert(elements, { Background = { AnsiColor = statusline_colors[cell_no] } })
      table.insert(elements, { Text = " " .. text .. sep })
      if not is_last then
        table.insert(elements, { Foreground = { AnsiColor = statusline_colors[cell_no + 1] } })
      end
      num_cells = num_cells + 1
    end

    while #cells > 0 do
      local cell = table.remove(cells, 1)
      push(cell, #cells == 0)
    end

    window:set_right_status(wezterm.format(elements))

    local left_elements = {}
    table.insert(left_elements, { Foreground = { AnsiColor = "White" } })
    table.insert(left_elements, { Background = { AnsiColor = "Black" } })
    table.insert(left_elements, { Text = string.format("%s § ", window:active_workspace()) })
    window:set_left_status(wezterm.format(left_elements))
  end)
end

return M
