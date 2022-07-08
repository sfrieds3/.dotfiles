P = function(v)
  print(vim.inspect(v))
  return v
end

Dump = function(...)
  local objects = vim.tbl_map(vim.inspect, {...})
  print(unpack(objects))
end
