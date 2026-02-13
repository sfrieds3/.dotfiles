local Statusline = {}
_G.Statusline = Statusline

local utils = require("utils.utils")

local get_mode = vim.api.nvim_get_mode
local get_current_win = vim.api.nvim_get_current_win
local get_window_buf = vim.api.nvim_win_get_buf
local buf_get_name = vim.api.nvim_buf_get_name
local fnamemodify = vim.fn.fnamemodify
local pathshorten = vim.fn.pathshorten

--- Set hl for use in the statusline
---@param ns integer namespace
---@param opts string[] opts for hl
function Statusline.set_statusline_hl(ns, opts)
  local statusline_bg = vim.api.nvim_get_hl(0, { name = "Statusline", link = false })["bg"]

  if opts["bg"] ~= nil then
    statusline_bg = opts["bg"]
  end
  vim.api.nvim_set_hl(ns, opts["name"], { fg = opts["fg"], bg = statusline_bg })
end

--- Get vcs info for buffer
---@param win_id integer Window id
---@return string vcs info for statusline
local function vcs(_win_id)
  local branch_sign = "λ"
  local git_info = vim.b.gitsigns_status_dict
  if not git_info or git_info.head == "" then
    return ""
  end
  local added = git_info.added and ("+" .. git_info.added .. " ") or ""
  local modified = git_info.changed and ("~" .. git_info.changed .. " ") or ""
  local removed = git_info.removed and ("-" .. git_info.removed .. " ") or ""
  local pad = ((added ~= "") or (removed ~= "") or (modified ~= "")) and " " or ""
  local diff_str = string.format("[%s%s%s%s]%s", pad, added, removed, modified, pad)
  local max_size = math.min(75, math.floor(0.33 * vim.o.columns))
  local git_str = string.format("%s(%s:%s)", diff_str, branch_sign, git_info.head)
  if string.len(git_str) > max_size then
    git_str = string.format("(%s:%s)", branch_sign, git_info.head)
  end
  return git_str
end

Statusline.diagnostic_levels = {
  Error = vim.diagnostic.severity.ERROR,
  Warn = vim.diagnostic.severity.WARN,
  Info = vim.diagnostic.severity.INFO,
  Hint = vim.diagnostic.severity.HINT,
}

--- Generate highlights for statusline
local function init_statusline_hl()
  -- diagnostics
  for k, _ in pairs(Statusline.diagnostic_levels) do
    local hl = vim.api.nvim_get_hl(0, { name = "DiagnosticSign" .. k, link = false })
    Statusline.set_statusline_hl(0, { name = "StatuslineDiagnosticSign" .. k, fg = hl["fg"] })
  end

  -- vcs
  Statusline.set_statusline_hl(
    0,
    { name = "StatuslineVcs", fg = vim.api.nvim_get_hl(0, { name = "@function", link = false })["fg"] }
  )

  -- line/col indicator
  local bg_color = vim.api.nvim_get_hl(0, { name = "Normal", link = false })["bg"]
    or vim.api.nvim_get_hl(0, { name = "NormalBg", link = false })["bg"]
  local blue_color = vim.api.nvim_get_hl(0, { name = "@function", link = false })["fg"]
  Statusline.set_statusline_hl(0, { name = "StatuslineLineCol", fg = bg_color, bg = blue_color })
end

--- Get running linters in buffer
---@return string list of running linters
local function get_linters()
  local linters = require("lint").get_running()
  if #linters == 0 then
    return ""
  end

  local unique_linters = setmetatable({}, utils.unique_table)
  for _, linter in ipairs(linters) do
    unique_linters[linter] = linter
  end
  return "  [" .. table.concat(unique_linters, ", ") .. "]"
end

--- Get lsp diagnostics for statusline
---@return string lsp diagnostics string
local function lsp_diagnostics()
  local status = vim.diagnostic.status()
  if #status > 0 then
    return "[ " .. status .. " ]"
  else
    return status
  end
end

function Statusline.init_lsp_progress()
  local ns = vim.api.nvim_create_augroup("sfrieds3:lsp_progress", {})
  vim.api.nvim_create_autocmd("LspProgress", {
    group = ns,
    pattern = "*",
    command = "redrawstatus",
  })
  vim.api.nvim_create_autocmd("User", {
    group = ns,
    pattern = "LspProgressStatusUpdated",
    command = "redrawstatus",
  })
end

--- Get lsp progress for use in statusline
---@return string? lsp status
function Statusline.lsp_progress()
  -- return require("lsp-progress").progress() .. " "

  return require("lsp-progress").progress({
    format = function(messages)
      local active_clients = vim.lsp.get_clients()
      local client_count = #active_clients
      local lsp_icon = ""
      if #messages > 0 then
        return lsp_icon .. " LSP:" .. client_count .. " " .. table.concat(messages, " ")
      end
      if #active_clients <= 0 then
        return lsp_icon .. " LSP:" .. client_count
      else
        local client_names = {}
        for _, client in ipairs(active_clients) do
          if client and client.name ~= "" and not vim.tbl_contains(client_names, client.name) then
            table.insert(client_names, client.name)
          end
        end
        return lsp_icon .. " LSP:" .. client_count .. " [" .. table.concat(client_names, ", ") .. "]"
      end
    end,
  })
end

local mode_table = {
  n = "Normal",
  no = "N·Operator Pending",
  v = "Visual",
  V = "V·Line",
  ["^V"] = "V·Block",
  s = "Select",
  S = "S·Line",
  ["^S"] = "S·Block",
  i = "Insert",
  ic = "Insert",
  R = "Replace",
  Rv = "V·Replace",
  c = "Command",
  cv = "Vim Ex",
  ce = "Ex",
  r = "Prompt",
  rm = "More",
  ["r?"] = "Confirm",
  ["!"] = "Shell",
  t = "Terminal",
  nt = "N·Terminal",
}

--- Return Formatted mode naem for use in statusline
---@param mode string Mode name
---@return string Mode name for use in statusline
local function mode_name(mode)
  return string.upper(mode_table[mode] or "Normal")
end

--- Get grapple icon
---@param icon_hi string icon highlight color
---@param filename_color string filename color
local function get_grapple_icon(icon_hi, filename_color)
  if require("grapple").exists() then
    local icon = require("grapple").name_or_index()
    return string.format("%%#%s#(󰛢 %s)%%#%s#", icon_hi, icon, filename_color)
  else
    return ""
  end
end

--- Return filename formatted for statusline
---@param buf_name string Buffer name
---@param win_id integer Id for window
---@param filename_color string color for filename
---@param shorten boolean? Should we shorten filename?
local function filename(buf_name, win_id, filename_color, shorten)
  local function format_filename(f)
    return " " .. f .. " "
  end
  shorten = shorten or true
  local base_name = fnamemodify(buf_name, [[:~:.]])
  local icon, hi, _ = require("mini.icons").get("file", buf_name)
  local file_icon = string.format("%%#%s# %s %%#%s#", hi, icon, filename_color)
  if shorten then
    local space = math.floor(0.5 * vim.o.columns)
    if string.len(base_name) <= space then
      return file_icon .. format_filename(base_name)
    else
      return file_icon .. format_filename(pathshorten(base_name))
    end
  else
    return file_icon .. format_filename(base_name)
  end
end

--- Add special symbol to denote file is modified
---@param modified boolean Is the file modified?
---@return string
local function set_modified_symbol(modified)
  if modified then
    return "[●] "
  else
    return ""
  end
end

--- Are we in paste mode or not?
---@return string boolean
local function get_paste()
  return vim.o.paste and "PASTE " or ""
end

--- Return statuline component if buffer is readonly
---@return string
local function get_readonly_space()
  return ((vim.o.paste and vim.bo.readonly) and " " or "") and "%r" .. (vim.bo.readonly and " " or "")
end

Statusline.format_string = ""
--- Build statusline format string
---@param section string Section definition for statusline
local function build_statusline(section)
  Statusline.format_string = Statusline.format_string .. section
end

build_statusline("%%#%s#") -- mode leftseparator
build_statusline("%%#%s#") -- mode_color
build_statusline("%s") -- mode_name
build_statusline("%%#%s#") -- mode leftseparator
build_statusline("%%#%s#") -- filename_color
build_statusline("%s") -- filename_segment
build_statusline("%s") -- modified symbol
build_statusline("%%#%s#") -- filetype_color
build_statusline("%s") -- filetype_segment
build_statusline("%%#%s# ") -- filename_color
build_statusline("%s") -- get_paste
build_statusline("%s") -- get_readonly_space
build_statusline("%s") -- lsp diagnostics
build_statusline("%s") -- linters
build_statusline("%%=")
build_statusline("%%<")
build_statusline(" %s ") -- lsp
build_statusline("%%#%s#") -- vcs color
build_statusline("%s") -- vcs
build_statusline("%s") -- line_col_segment

local function generate_inverse_hi(base_highlight_name, bg_override)
  local inverse_color_name = string.format("%s_inverse", base_highlight_name)
  local color = vim.api.nvim_get_hl(0, { name = base_highlight_name, link = false })
  vim.api.nvim_set_hl(0, inverse_color_name, { fg = color["bg"], bg = bg_override or color["fg"] })
  return inverse_color_name
end

local statuslines = {}
--- Generate the statusline string
---@return string The statusline string
function Statusline.status()
  local win_id = vim.g.statusline_winid
  if win_id == get_current_win() or statuslines[win_id] == nil then
    local mode = get_mode().mode
    local buf_nr = get_window_buf(win_id)
    local bufname = buf_get_name(buf_nr)
    local colors = {
      -- mode_color: @type, Search, Substitute
      mode_color = "Substitute",
      filename_color = "StatusLine",
      filetype_color = "StatusLine",
      vcs_color = "StatuslineVcs",
      line_col_color = "StatuslineLineCol",
    }
    local inverse_colors = {
      line_col_color_inverse = generate_inverse_hi(colors.line_col_color),
      mode_color_inverse = generate_inverse_hi(
        colors.mode_color,
        vim.api.nvim_get_hl(0, { name = "Statusline", link = false })["bg"]
      ),
    }
    local filename_segment = filename(bufname, win_id, colors.filename_color)
    local filetype_segment = "%y"
    local line_col_segment = filename_segment ~= ""
        and " %#" .. inverse_colors.line_col_color_inverse .. "#%#" .. colors.line_col_color .. "#ℓ:%l 𝚌:%c%#" .. inverse_colors.line_col_color_inverse .. "#"
      or " "
    statuslines[win_id] = string.format(
      Statusline.format_string,
      inverse_colors.mode_color_inverse,
      colors.mode_color,
      mode_name(mode),
      inverse_colors.mode_color_inverse,
      colors.filename_color,
      filename_segment,
      set_modified_symbol(vim.bo.modified),
      colors.filetype_color,
      filetype_segment,
      colors.filename_color,
      get_paste(),
      get_readonly_space(),
      lsp_diagnostics(),
      get_linters(),
      Statusline.lsp_progress(),
      colors.vcs_color,
      vcs(win_id),
      line_col_segment
    )
  end

  return statuslines[win_id]
end

-- init statusline after everything loaded
vim.api.nvim_create_autocmd({ "VimEnter", "Colorscheme" }, {
  group = vim.api.nvim_create_augroup("sfrieds3:statusline_init", {}),
  pattern = "*",
  callback = function()
    init_statusline_hl()
    vim.o.statusline = "%!v:lua.Statusline.status()" -- *v:lua-call*
  end,
})

return Statusline
