local disable_auto_format_files = { ".pynoautoformat", ".pydisableautoformat", ".pydisableformat" }

local python_dir_markers = { "pyprojec.toml", "setup.py", "setup.cfg", ".git" }

local project_file = vim.fn.expand("%")
local project_root = vim.fs.root(project_file, python_dir_markers)
P(vim.fs.find(disable_auto_format_files, { path = project_root, type = "file", limit = 1, upward = true }))
