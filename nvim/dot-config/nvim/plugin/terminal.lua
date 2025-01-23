local augroup = require("utils.utils").augroup

vim.api.nvim_create_autocmd("TermOpen", {
  group = augroup("term-open"),
  callback = function()
    if vim.startswith(vim.api.nvim_buf_get_name(0), "term://") then
      vim.opt_local.number = false
      vim.opt_local.relativenumber = false
      vim.opt_local.listchars = {}
      vim.opt_local.scrolloff = 0
      vim.cmd("startinsert")
    end
  end,
})

local esc_timer = vim.uv.new_timer()

vim.api.nvim_create_autocmd({ "TermEnter" }, {
  group = augroup("term-map"),
  callback = function()
    local buf_keymap_opts = { buffer = true }
    vim.keymap.set("t", "<esc>", function()
      if esc_timer:is_active() then
        esc_timer:stop()
        vim.cmd("stopinsert")
      else
        esc_timer:start(200, 0, function() end)
        return "<esc>"
      end
    end, buf_keymap_opts)
  end,
})
