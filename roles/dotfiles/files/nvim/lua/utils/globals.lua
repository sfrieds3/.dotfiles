P = function(v)
  print(vim.inspect(v))
  return v
end

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
