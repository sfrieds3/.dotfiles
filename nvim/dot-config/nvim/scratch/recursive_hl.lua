-- Recursively iterate through highlight until
-- we get the real value
---@param name string highlight name
local function recursive_get_highlight(name)
  local hl = vim.api.nvim_get_hl(0, { name = name, link = false })
  P(hl)
  if hl["link"] then
    recursive_get_highlight(hl["link"])
  else
    return hl
  end
end

local hl = recursive_get_highlight("@namespace")
P(hl)
