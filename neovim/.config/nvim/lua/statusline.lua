local lsp_status = require('lsp-status')

local get_mode = vim.api.nvim_get_mode
local get_current_win = vim.api.nvim_get_current_win
local get_window_buf = vim.api.nvim_win_get_buf
local buf_get_name = vim.api.nvim_buf_get_name
local fnamemodify = vim.fn.fnamemodify
local get_window_width = vim.api.nvim_win_get_width
local pathshorten = vim.fn.pathshorten

--local M = {}

--StatusLine = setmetatable(M, {
--  __call = function(statusline, mode)
--    if statusline == 'active' then return statusline:active() end
--    if statusline == 'inactive' then return statusline:inactive() end
--    if statusline == 'dirvish' then return statusline:dirvish() end
--  end
--})

--return statuslines[win_id]

local function vcs()
  local branch_sign = 'λ'
  local git_info = vim.b.gitsigns_status_dict
  if not git_info or git_info.head == '' then
    return ''
  end
  local added = git_info.added and ('+' .. git_info.added .. ' ') or ''
  local modified = git_info.changed and ('~' .. git_info.changed .. ' ') or ''
  local removed = git_info.removed and ('-' .. git_info.removed .. ' ') or ''
  local pad = ((added ~= '') or (removed ~= '') or (modified ~= '')) and ' ' or ''
  local diff_str = string.format('%s%s%s%s', added, removed, modified, pad)
  return string.format('%s(%s:%s) ', diff_str, branch_sign, git_info.head)
end

local function lint_lsp(buf)
  local result = ''
  if #vim.lsp.buf_get_clients(buf) > 0 then
    result = result .. lsp_status.status()
  end
  return result
end

local mode_table = {
  n = 'Normal',
  no = 'N·Operator Pending',
  v = 'Visual',
  V = 'V·Line',
  ['^V'] = 'V·Block',
  s = 'Select',
  S = 'S·Line',
  ['^S'] = 'S·Block',
  i = 'Insert',
  ic = 'Insert',
  R = 'Replace',
  Rv = 'V·Replace',
  c = 'Command',
  cv = 'Vim Ex',
  ce = 'Ex',
  r = 'Prompt',
  rm = 'More',
  ['r?'] = 'Confirm',
  ['!'] = 'Shell',
  t = 'Terminal',
  nt = 'N·Terminal',
}

local function mode_name(mode)
  return string.upper(mode_table[mode] or 'Normal')
end

local function filename(buf_name, win_id)
  local base_name = fnamemodify(buf_name, [[:~:.]])
  local space = math.min(50, math.floor(0.4 * get_window_width(win_id)))
  if string.len(base_name) <= space then
    return base_name
  else
    return pathshorten(base_name)
  end
end

local function update_colors(mode)
  local mode_color = 'StatuslineMiscAccent'
  if mode == 'n' then
    mode_color = 'StatuslineNormalAccent'
  elseif mode == 'i' or mode == 'ic' then
    mode_color = 'StatuslineInsertAccent'
  elseif mode == 'R' then
    mode_color = 'StatuslineReplaceAccent'
  elseif mode == 'c' then
    mode_color = 'StatuslineConfirmAccent'
  elseif mode == 't' then
    mode_color = 'StatuslineTerminalAccent'
  else
    mode_color = 'StatuslineMiscAccent'
  end

  local filename_color
  local filetype_color
  if vim.bo.modified then
    filename_color = 'StatuslineFilenameModified'
    filetype_color = 'StatuslineFiletypeModified'
  else
    filename_color = 'StatuslineFilenameNoMod'
    filetype_color = 'StatuslineFiletypeNoMod'
  end

  return mode_color, filename_color, filetype_color
end

local function set_modified_symbol(modified)
  if modified then
    vim.cmd [[hi StatuslineModified guibg=#3a3a3a gui=bold guifg=#d75f5f]]
    return '  ●'
  else
    vim.cmd [[ hi StatuslineModified guibg=#3a3a3a gui=bold guifg=#afaf00]]
    return ''
  end
end

local function get_paste()
  return vim.o.paste and 'PASTE ' or ''
end

local function get_readonly_space()
  return ((vim.o.paste and vim.bo.readonly) and ' ' or '') and '%r' .. (vim.bo.readonly and ' ' or '')
end

local statusline_format =
  '%%#%s# %s %%#StatuslineModified#%s%%#%s# %s %%#%s#%s%s%%<%%#%s# %s%s%%<%%#%s#%s%%<%%=%%#StatuslineVC#%s %%#StatuslineLint#%s%%#StatuslineFiletype#'

local statuslines = {}
local function status()
  local win_id = vim.g.statusline_winid
  if win_id == get_current_win() or statuslines[win_id] == nil then
    local mode = get_mode().mode
    local buf_nr = get_window_buf(win_id)
    local bufname = buf_get_name(buf_nr)
    local filename_segment = filename(bufname, win_id)
    local filetype_segment = '%y'
    local treesitter_segment = vim.fn['nvim_treesitter#statusline']({ type_patterns = { "function" } })
    local mode_color, filename_color, filetype_color = update_colors(mode)
    local line_col_segment = filename_segment ~= '' and ' %#StatuslineLineCol#| %l:%#StatuslineLineCol#%c |' or ''
    statuslines[win_id] = string.format(
      statusline_format,
      mode_color,
      mode_name(mode),
      set_modified_symbol(vim.bo.modified),
      filename_color,
      filename_segment,
      filetype_color,
      filetype_segment,
      line_col_segment,
      filename_color,
      get_paste(),
      get_readonly_space(),
      filetype_color,
      treesitter_segment,
      vcs(),
      lint_lsp(buf_nr)
    )
  else
    -- print(vim.g.statusline_winid, win_getid(winnr()))
  end

  return statuslines[win_id]
end

return { status = status }

