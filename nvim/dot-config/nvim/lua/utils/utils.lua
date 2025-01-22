local Utils = {}

Utils.unique_table = {
  __newindex = function(table, key, val)
    if not rawget(table, key) then
      rawset(table, #table + 1, val)
      rawset(table, key, true)
    end
  end,
}

function Utils.remove_key(tbl, key)
  local elem = tbl[key]
  tbl[key] = nil
  return elem
end

function Utils.t(str)
  -- Adjust boolean arguments as needed
  return vim.api.nvim_replace_termcodes(str, true, true, true)
end

function Utils.table_contains(table, key)
  for _, value in pairs(table) do
    if value == key then
      return true
    end
  end
  return false
end

function Utils.mapper(opts)
  -- return function with default opts to map keybindings
  -- opts default to noremap if opts=nil
  opts = opts or { noremap = true }
  local map = function(mode, map, cmd)
    vim.api.nvim_set_keymap(mode, map, cmd, opts)
  end
  return map
end

function Utils.get_buf_option(opt)
  local status_ok, buf_option = pcall(vim.api.nvim_buf_get_option, 0, opt)
  if not status_ok then
    return nil
  else
    return buf_option
  end
end

-- Recursively iterate through highlight until
-- we do not return a link
---@param name string highlight name
function Utils.recursive_get_highlight(name)
  local hl = vim.api.nvim_get_hl(0, { name = name, link = false })
  if hl["link"] then
    Utils.recursive_get_highlight(hl["link"])
  else
    return hl
  end
end

_G.sfrieds3_autocmd_registry = {}

function Utils.augroup(name, check_dupe)
  local group_name = "sfrieds3:" .. name
  local should_check_dupe = check_dupe or true
  if should_check_dupe then
    local err, _ = pcall(vim.api.nvim_get_autocmds, { group = group_name })
    if err then
      print("augroup ", group_name, " already exists, bailing.")
      return
    end
  end

  local augroup = vim.api.nvim_create_augroup(group_name, { clear = true })

  if not Utils.table_contains(_G.sfrieds3_autocmd_registry, augroup) then
    _G.sfrieds3_autocmd_registry[augroup] = true
  end

  return augroup
end

--- Gets visual selection on a single line
--- At some point, should make this work across lines.. but I'm too lazy right now
---@param bufnr? integer buffer number
---@return string visual selection string
function Utils.get_visual_selection(bufnr)
  bufnr = bufnr or 0
  local visual_begin = vim.api.nvim_buf_get_mark(0, "<")
  local visual_end = vim.api.nvim_buf_get_mark(0, ">")

  local lines = vim.api.nvim_buf_get_text(0, visual_begin[1] - 1, visual_begin[2], visual_end[1] - 1, visual_end[2], {})

  return lines[1]
end

return Utils
