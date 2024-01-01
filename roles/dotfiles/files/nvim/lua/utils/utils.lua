local M = {}
local vo = vim.o

function M.remove_key(tbl, key)
  local elem = tbl[key]
  tbl[key] = nil
  return elem
end

function M.opt(o, v, scopes)
  scopes = scopes or { vo }
  for _, s in ipairs(scopes) do
    s[o] = v
  end
end

function M.t(str)
  -- Adjust boolean arguments as needed
  return vim.api.nvim_replace_termcodes(str, true, true, true)
end

function M.mapper(opts)
  -- return function with default opts to map keybindings
  -- opts default to noremap if opts=nil
  opts = opts or { noremap = true }
  local map = function(mode, map, cmd)
    vim.api.nvim_set_keymap(mode, map, cmd, opts)
  end
  return map
end

function M.source_dir(dir)
  -- inspiration: https://github.com/tjdevries/astronauta.nvim/blob/master/lua/astronauta/plugin.lua
  local source_path = string.format("%s/**/*.lua", dir)
  for _, mod in ipairs(vim.api.nvim_get_runtime_file(source_path, true)) do
    local ok, msg = pcall(loadfile, mod)
    if not ok then
      print("Failed to load: ", mod)
      print("\t", msg)
    end
  end
end

function M.isempty(s)
  return s == nil or s == ""
end

function M.get_buf_option(opt)
  local status_ok, buf_option = pcall(vim.api.nvim_buf_get_option, 0, opt)
  if not status_ok then
    return nil
  else
    return buf_option
  end
end

-- from folke/LazyVim
-- returns the root directory based on:
-- * lsp workspace folders
-- * lsp root_dir
-- * root pattern of filename of the current buffer
-- * root pattern of cwd
---@return string
function M.get_root()
  ---@type string?
  local path = vim.api.nvim_buf_get_name(0)
  path = path ~= "" and vim.loop.fs_realpath(path) or nil
  ---@type string[]
  local roots = {}
  if path then
    for _, client in pairs(vim.lsp.get_clients({ bufnr = 0 })) do
      local workspace = client.config.workspace_folders
      local paths = workspace and vim.tbl_map(function(ws)
        return vim.uri_to_fname(ws.uri)
      end, workspace) or client.config.root_dir and { client.config.root_dir } or {}
      for _, p in ipairs(paths) do
        local r = vim.loop.fs_realpath(p)
        if path:find(r, 1, true) then
          roots[#roots + 1] = r
        end
      end
    end
  end
  table.sort(roots, function(a, b)
    return #a > #b
  end)
  ---@type string?
  local root = roots[1]
  if not root then
    path = path and vim.fs.dirname(path) or vim.loop.cwd()
    ---@type string?
    root = vim.fs.find(M.root_patterns, { path = path, upward = true })[1]
    root = root and vim.fs.dirname(root) or vim.loop.cwd()
  end
  ---@cast root string
  return root
end

-- Recursively iterate through highlight until
-- we do not return a link
---@param name string highlight name
function M.recursive_get_highlight(name)
  local hl = vim.api.nvim_get_hl(0, { name = name, link = false })
  if hl["link"] then
    M.recursive_get_highlight(hl["link"])
  else
    return hl
  end
end

function M.augroup(name, check_dupe)
  local group_name = "sfrieds3:" .. name
  local should_check_dupe = check_dupe or true
  if should_check_dupe then
    local err, _ = pcall(vim.api.nvim_get_autocmds, { group = group_name })
    if err then
      print("augroup ", group_name, " already exists, bailing.")
      return
    end
  end

  return vim.api.nvim_create_augroup(group_name, { clear = true })
end

return M
