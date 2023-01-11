local tbl = { desc = "test description", other = "other_test" }

local function remove_key(t, key)
  local elem = t[key]
  t[key] = nil
  return elem
end

local removed = remove_key(tbl, "nope")

P(removed)
