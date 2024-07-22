-- use with package.loaded[name] = nil
-- to force reloading of modules
FORCE_RELOAD = true

--- Pretty print object (runs vim.inspect under the hood)
---@param v any variable to print
---@return any v
P = function(v)
  print(vim.inspect(v))
  return v
end

--- Pretty print nested table
---@vararg table[any] nested table to pretty print
DUMP = function(...)
  local objects = vim.tbl_map(vim.inspect, { ... })
  print(unpack(objects))
end

---@alias Logger table<string, table>

---@type Logger
local loggers = {}

--- Get a plenary logger for level
---@param level string? log level
---@return Logger
LOGGER = function(level)
  level = level or "info"
  local _logger = loggers[level]
  if not _logger then
    _logger = require("plenary.log"):new({ level = level })
    loggers[level] = _logger
  end

  return _logger
end
