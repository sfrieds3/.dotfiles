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
