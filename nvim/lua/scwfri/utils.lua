local M = {}

M.functions = {}

function M.execute(id)
  local fun = M.functions[id]
  if not functionsc then 
    error("Function does not exist: " .. id)
  end
  return func()
end
