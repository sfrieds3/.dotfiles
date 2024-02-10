local M = {}
_G.Status = M

---@return {name:string, text:string, texthl:string}[]
function M.get_signs(winid)
  winid = winid or vim.g.statusline_winid
  print(winid)
  local buf = vim.api.nvim_win_get_buf(winid)
  return vim.tbl_map(function(sign)
    return vim.fn.sign_getdefined(sign.name)[1]
  end, vim.fn.sign_getplaced(buf, { group = "*", lnum = vim.v.lnum })[1].signs)
end

function M.column()
  local sign, git_sign
  for _, s in ipairs(M.get_signs()) do
    if s.name:find("GitSign") then
      git_sign = s
    else
      sign = s
    end
  end
  local components = {
    sign and ("%#" .. sign.texthl .. "#" .. sign.text .. "%*") or " ",
    [[%=]],
    [[%{&nu?(&rnu&&v:relnum?v:relnum:v:lnum):''} ]],
    git_sign and ("%#" .. git_sign.texthl .. "#" .. git_sign.text .. "%*") or "  ",
  }

  -- %s%=%{v:wrap ? "" : v:lnum} %#FoldColumn#%@v:lua.StatusColumn.handler.fold@%{v:lua.StatusColumn.display.fold()}%#StatusColumnBorder#▐%#StatusColumnBuffer#
  return table.concat(components, "")
end

vim.api.nvim_create_autocmd("Colorscheme", {
  group = vim.api.nvim_create_augroup("sfrieds3:statuscol_init", {}),
  pattern = "*",
  callback = function()
    -- vim.opt.statuscolumn = [[%!v:lua.Status.column()]]
  end,
})

return M
