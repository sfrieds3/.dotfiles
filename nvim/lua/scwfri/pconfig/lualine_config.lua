require('lualine').setup {
  options = {
    theme = 'auto',
    icons_enabled = false,
  },
  sections = {
    lualine_c = {
      {
        'filename',
        file_status = true,
        path = 1
      }
    }
  }
}
