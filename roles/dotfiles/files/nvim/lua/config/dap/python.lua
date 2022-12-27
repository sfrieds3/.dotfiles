require("dap-python").setup("~/.venv/venv/bin/python")

-- django configuration
table.insert(require("dap").configurations.python, {
  type = "python",
  request = "launch",
  name = "Django",
  program = vim.fn.getcwd() .. "/manage.py",
  args = { "runserver", "--noreload" },
})
