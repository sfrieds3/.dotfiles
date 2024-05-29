local wezterm = require("wezterm")
local M = {}

--- Get folder from a path-like string
---@param pwd string path-like string
---@return string
local function get_folder(pwd)
  if pwd == nil then
    return nil
  end

  local split_path = {}
  local sep = "/"
  for segment in string.gmatch(pwd, "[^" .. sep .. "]+") do
    table.insert(split_path, segment)
  end

  return split_path[#split_path]
end

--- Return current working directory of tab
---@param tab table tab details
---@return string
local function get_current_working_dir(tab)
  ---@type table
  local current_dir = tab.active_pane.current_working_dir or { path = "" }
  return get_folder(current_dir.path)
end

--- Return index of tab
---@param tab table tab details
---@return integer
local function get_tab_index(tab)
  return tonumber(tab.tab_index) + 1
end

---@type table
local process_icons = {
  ["docker"] = wezterm.nerdfonts.linux_docker,
  ["docker-compose"] = wezterm.nerdfonts.linux_docker,
  ["psql"] = wezterm.nerdfonts.dev_postgresql,
  ["kuberlr"] = wezterm.nerdfonts.linux_docker,
  ["kubectl"] = wezterm.nerdfonts.linux_docker,
  ["stern"] = wezterm.nerdfonts.linux_docker,
  ["nvim"] = wezterm.nerdfonts.custom_vim,
  ["make"] = wezterm.nerdfonts.seti_makefile,
  ["vim"] = wezterm.nerdfonts.dev_vim,
  ["go"] = wezterm.nerdfonts.seti_go,
  ["fish"] = wezterm.nerdfonts.md_fish,
  ["zsh"] = wezterm.nerdfonts.dev_terminal,
  ["bash"] = wezterm.nerdfonts.cod_terminal_bash,
  ["btm"] = wezterm.nerdfonts.mdi_chart_donut_variant,
  ["htop"] = wezterm.nerdfonts.mdi_chart_donut_variant,
  ["cargo"] = wezterm.nerdfonts.dev_rust,
  ["sudo"] = wezterm.nerdfonts.fa_hashtag,
  ["lazydocker"] = wezterm.nerdfonts.linux_docker,
  ["git"] = wezterm.nerdfonts.dev_git,
  ["lua"] = wezterm.nerdfonts.seti_lua,
  ["wget"] = wezterm.nerdfonts.mdi_arrow_down_box,
  ["curl"] = wezterm.nerdfonts.mdi_flattr,
  ["gh"] = wezterm.nerdfonts.dev_github_badge,
  ["ruby"] = wezterm.nerdfonts.cod_ruby,
  ["pwsh"] = wezterm.nerdfonts.seti_powershell,
  ["node"] = wezterm.nerdfonts.dev_nodejs_small,
  ["dotnet"] = wezterm.nerdfonts.md_language_csharp,
}

--- Get icon for current process in tabs
---@param tab table tab details
---@return string
local function get_process(tab)
  return tab.active_pane.foreground_process_name:match("([^/\\]+)$")
end

--- Get icon for current process in tabs
---@param process string tab details
---@return string
local function get_process_icon(process_name, default_nerdfont)
  local nerdfont = default_nerdfont or "cod_terminal"
  local icon = process_icons[process_name] or wezterm.nerdfonts[nerdfont]

  return icon
end

---@return action_callback
function M.setup()
  local process_exclude_unseen_output = {
    "nvim",
    "vim",
  }
  wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
    local tab_title_text =
      string.format(" %s: %s %s ", get_tab_index(tab), get_process_icon(get_process(tab)), get_current_working_dir(tab))

    -- local has_unseen_output = false
    -- for _, pane in ipairs(tab.panes) do
    --   local proc = get_process(tab)
    --   if pane.has_unseen_output and not process_exclude_unseen_output[proc] ~= nil then
    --     has_unseen_output = true
    --     break
    --   end
    -- end
    -- if has_unseen_output then
    --   return wezterm.format({
    --     { Attribute = { Intensity = "Bold" } },
    --     { Foreground = { AnsiColor = "Black" } },
    --     { Background = { AnsiColor = "Red" } },
    --     { Text = tab_title_text },
    --   })
    -- end

    return wezterm.format({
      { Attribute = { Intensity = "Bold" } },
      { Text = tab_title_text },
    })
  end)
end

return M
