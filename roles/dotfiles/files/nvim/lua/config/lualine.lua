local function linecol_segment()
  return 'ℓ:%l 𝚌:%c'
end

require('lualine').setup {
  options = {
    icons_enabled = true,
    theme = 'auto',
    component_separators = { left = '', right = ''},
    section_separators = { left = '', right = ''},
    disabled_filetypes = {},
    always_divide_middle = true,
    globalstatus = true,
  },
  sections = {
    lualine_a = {'mode'},
    lualine_b = {
      { 'branch', icon = 'λ' },
        'diff',
        'diagnostics'
    },
    lualine_c = {
      { 'filename',
        path = 1,
        symbols = {
          modified = ' ●',
          readonly = ' ✘',
        },
      },
    },
    lualine_x = {'encoding', 'fileformat', 'filetype'},
    lualine_y = {'progress'},
    lualine_z = { linecol_segment }
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {
      { 'filename',
        path = 2,
      },
    },
    lualine_x = {'location'},
    lualine_y = {},
    lualine_z = {}
  },
  tabline = {
    lualine_a = {'buffers'},
    lualine_b = {
      { 'filename',
        path = 0,
        symbols = {
          modified = ' ●',
          readonly = ' ✘',
        },
      },
    },
    lualine_c = {},
    lualine_x = {},
    lualine_y = {},
    lualine_z = {'tabs'}
  },
  extensions = { 'aerial', 'nvim-tree', 'symbols-outline', 'quickfix', 'fugitive' },
}
