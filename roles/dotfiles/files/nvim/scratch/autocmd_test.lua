local function augroup(name, check_dupe)
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

augroup("test")
augroup("test")
