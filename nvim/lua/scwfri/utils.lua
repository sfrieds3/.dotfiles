local M = {}

function _G.dump(...)
  local objects = vim.tbl_map(vim.inspect, {...})
  print(unpack(objects))
end

function M.t(str)
    -- Adjust boolean arguments as needed
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end

return M
