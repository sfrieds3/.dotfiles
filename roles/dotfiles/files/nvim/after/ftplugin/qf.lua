vim.keymap.set("n", "j", "j", { buffer = true })
vim.keymap.set("n", "k", "k", { buffer = true })
vim.keymap.set("n", "gj", "gj", { buffer = true })
vim.keymap.set("n", "gk", "gk", { buffer = true })
vim.keymap.set("n", "H", ":colder<CR>", { buffer = true })
vim.keymap.set("n", "L", ":cnewer<CR>", { buffer = true })

-- open qf in trouble
vim.keymap.set("n", "<leader>ct", function()
  vim.schedule(function()
    vim.cmd([[cclose]])
    vim.cmd([[Trouble qflist open]])
  end)
end, { buffer = true })

-- use entire screen width for qf window
vim.cmd([[wincmd J]])

-- set default qf height to 1/3 of window
vim.cmd([[let &l:winheight = &lines / 3]])
