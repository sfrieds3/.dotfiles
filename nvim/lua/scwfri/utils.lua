local M = {}

M.functions = {}

function M.execute(id)
  local func = M.functions[id]
  if not func then 
    error("Function does not exist: " .. id)
  end
  return func()
end

function _G.dump(...)
  local objects = vim.tbl_map(vim.inspect, {...})
  print(unpack(objects))
end

local function M.t(str)
    -- Adjust boolean arguments as needed
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end
