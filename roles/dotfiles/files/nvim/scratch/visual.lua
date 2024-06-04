-- this is a test

function get_visual()
  local visual_begin = vim.api.nvim_buf_get_mark(0, "<")
  local visual_end = vim.api.nvim_buf_get_mark(0, ">")

  local lines = vim.api.nvim_buf_get_text(0, visual_begin[1] - 1, visual_begin[2], visual_end[1] - 1, visual_end[2], {})

  P(lines)
end
