require("bqf").setup({
  auto_enable = true,
  magic_window = true,
  auto_resize_height = false,
  preview = {
    auto_preview = false,
  },
  filter = {
    fzf = {
      action_for = {
        ['ctrl-x'] = 'split',
        ['ctrl-t'] = 'tabedit',
        ['ctrl-v'] = 'vsplit',
        ['ctrl-q'] = 'signtoggle'
      },
    },
  },
})
