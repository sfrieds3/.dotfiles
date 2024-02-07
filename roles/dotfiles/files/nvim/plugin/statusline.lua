local Statusline = {}
_G.Statusline = Statusline

local get_mode = vim.api.nvim_get_mode
local get_current_win = vim.api.nvim_get_current_win
local get_window_buf = vim.api.nvim_win_get_buf
local buf_get_name = vim.api.nvim_buf_get_name
local fnamemodify = vim.fn.fnamemodify
local get_window_width = vim.api.nvim_win_get_width
local pathshorten = vim.fn.pathshorten

local function vcs(win_id)
  local branch_sign = "λ"
  local git_info = vim.b.gitsigns_status_dict
  if not git_info or git_info.head == "" then
    return ""
  end
  local added = git_info.added and ("+" .. git_info.added .. " ") or ""
  local modified = git_info.changed and ("~" .. git_info.changed .. " ") or ""
  local removed = git_info.removed and ("-" .. git_info.removed .. " ") or ""
  local pad = ((added ~= "") or (removed ~= "") or (modified ~= "")) and " " or ""
  local diff_str = string.format("%s%s%s%s", added, removed, modified, pad)
  local max_size = math.min(75, math.floor(0.33 * get_window_width(win_id)))
  local git_str = string.format("%s(%s:%s) |", diff_str, branch_sign, git_info.head)
  if string.len(git_str) > max_size then
    git_str = string.format("(%s:%s) |", branch_sign, git_info.head)
  end
  return git_str
end

local function lsp()
  local count = {}
  local levels = {
    errors = "Error",
    warnings = "Warn",
    info = "Info",
    hints = "Hint",
  }

  local has_diagnostics = false
  for k, level in pairs(levels) do
    has_diagnostics = true
    count[k] = vim.tbl_count(vim.diagnostic.get(0, { severity = level }))
  end

  local errors = ""
  local warnings = ""
  local hints = ""
  local info = ""

  -- TODO: set background using `vim.api.nvim_get_hl(0, { name = "DiagnosticSignError"})`
  -- this returns:
  -- {bg = 1315860, fg = 12483709}
  -- then use vim.api.nvim_set_hl to set this with statusline bg, diagnostic color fg
  if count["errors"] ~= 0 then
    local diagnostic = vim.fn.sign_getdefined("DiagnosticSignError")[1]
    errors = " %#" .. diagnostic.texthl .. "#" .. diagnostic.text .. count["errors"]
  end
  if count["warnings"] ~= 0 then
    local diagnostic = vim.fn.sign_getdefined("DiagnosticSignWarn")[1]
    warnings = " %#" .. diagnostic.texthl .. "#" .. diagnostic.text .. count["warnings"]
  end
  if count["hints"] ~= 0 then
    local diagnostic = vim.fn.sign_getdefined("DiagnosticSignInfo")[1]
    hints = " %#" .. diagnostic.texthl .. "#" .. diagnostic.text .. count["hints"]
  end
  if count["info"] ~= 0 then
    local diagnostic = vim.fn.sign_getdefined("DiagnosticSignHint")[1]
    info = " %#" .. diagnostic.texthl .. "#" .. diagnostic.text .. count["info"]
  end

  return errors .. warnings .. hints .. info .. "%#statusline#"
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

local function mode_name(mode)
  return string.upper(mode_table[mode] or "Normal")
end

local function filename(buf_name, win_id, shorten)
  local function format_filename(f)
    return " " .. f .. " "
  end
  shorten = shorten or false
  local base_name = fnamemodify(buf_name, [[:~:.]])
  if shorten then
    local space = math.min(50, math.floor(0.5 * get_window_width(win_id)))
    if string.len(base_name) <= space then
      return format_filename(base_name)
    else
      return format_filename(pathshorten(base_name))
    end
  else
    return format_filename(base_name)
  end
end

local function set_modified_symbol(modified)
  if modified then
    return "[●] "
  else
    return ""
  end
end

local function get_paste()
  return vim.o.paste and "PASTE " or ""
end

local function get_readonly_space()
  return ((vim.o.paste and vim.bo.readonly) and " " or "") and "%r" .. (vim.bo.readonly and " " or "")
end

local statusline_format = ""
local function build_statusline(section)
  statusline_format = statusline_format .. section
end

build_statusline("%%#%s#") -- mode_color
build_statusline(" %s ") -- mode_name
build_statusline("%%#%s#") -- filename_color
build_statusline("%s") -- filename_segment
build_statusline("%s") -- modified symbol
build_statusline("%%#%s#") -- filetype_color
build_statusline("%s") -- filetype_segment
build_statusline("%%<")
build_statusline("%%#%s# ") -- filename_color
build_statusline("%s") -- get_paste
build_statusline("%s") -- get_readonly_space
build_statusline("%s") -- lsp
build_statusline("%%<")
build_statusline("%%<")
build_statusline("%%=")
build_statusline("%%#%s#") -- vcs
build_statusline("%s") -- vcs
build_statusline("%s") -- line_col_segment

local statuslines = {}
function Statusline.status()
  local win_id = vim.g.statusline_winid
  if win_id == get_current_win() or statuslines[win_id] == nil then
    local mode = get_mode().mode
    local buf_nr = get_window_buf(win_id)
    local bufname = buf_get_name(buf_nr)
    local filename_segment = filename(bufname, win_id)
    local filetype_segment = "%y"
    local mode_color, filename_color, filetype_color, vcs_color = "@type", "statusline", "statusline", "@function"
    local line_col_segment = filename_segment ~= "" and "%#@namespace# ℓ:%l 𝚌:%c " or " "
    statuslines[win_id] = string.format(
      statusline_format,
      mode_color,
      mode_name(mode),
      filename_color,
      filename_segment,
      set_modified_symbol(vim.bo.modified),
      filetype_color,
      filetype_segment,
      filename_color,
      get_paste(),
      get_readonly_space(),
      lsp(),
      vcs_color,
      vcs(win_id),
      line_col_segment
    )
  else
    -- print(vim.g.statusline_winid, win_getid(winnr()))
  end

  return statuslines[win_id]
end

vim.o.statusline = "%!v:lua.Statusline.status()" -- *v:lua-call*

return Statusline
