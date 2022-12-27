require("dap-python").setup("~/.venv/venv/bin/python")

-- django configuration
table.insert(require("dap").configurations.python, {
  type = "python",
  request = "launch",
  name = "Django",
  program = vim.fn.getcwd() .. "/manage.py",
  args = { "runserver", "--noreload" },
})

vim.keymap.set("n", "\\dpr", function()
  local test_runner = vim.fn.input("Choose test_runner: ")
  require("dap-python").test_runner = test_runner
end, { desc = "dap-python: choose test_runner" })

-- TODO: only set this in python files
vim.keymap.set("n", "\\tf", function()
  require("dap-python").test_method()
end, { desc = "dap-python: test function" })
