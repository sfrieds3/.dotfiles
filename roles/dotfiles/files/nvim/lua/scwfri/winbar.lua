local M = {}

M.winbar_filetype_exclude = {
  'help',
  'startify',
  'dashboard',
  'packer',
  'neogitstatus',
  'NvimTree',
  'Trouble',
  'alpha',
  'lir',
  'Outline',
  'spectre_panel',
  'toggleterm',
}

local get_filename = function()
  local filename = vim.fn.expand '%:p'
  local extension = vim.fn.expand '%:e'
  local f = require('utils.utils')

  if not f.isempty(filename) then
    local file_icon, file_icon_color = require('nvim-web-devicons').get_icon_color(
      filename,
      extension,
      { default = true }
    )

    local hl_group = 'FileIconColor' .. extension

    vim.api.nvim_set_hl(0, hl_group, { fg = file_icon_color })
    if f.isempty(file_icon) then
      file_icon = ''
      file_icon_color =''
    end

    return ' ' .. '%#' .. hl_group .. '#' .. file_icon .. '%*' .. ' ' .. '%#LineNr#' .. filename .. '%*'
  else
    return ''
  end
end

local excludes = function()
  if vim.tbl_contains(M.winbar_filetype_exclude, vim.bo.filetype) then
    vim.opt_local.winbar = nil
    return true
  end
  return false
end

M.get_winbar = function()
  if excludes() then
    return
  end
  local value = get_filename()

  local f = require('utils.utils')
  if not f.isempty(value) and f.get_buf_option 'mod' then
    local mod = '%#LineNr#' .. ' ●' .. '%*'
    value = value .. mod
  end

  local winbar_format = string.format('%%=%s', value)
  local status_ok, _ = pcall(vim.api.nvim_set_option_value, 'winbar', winbar_format, { scope = 'local' })
  if not status_ok then
    return
  end
end

return M
