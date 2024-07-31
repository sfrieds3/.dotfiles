local is_executable = vim.fn.executable
local bufnr = vim.api.nvim_get_current_buf()

vim.g.python_highlight_space_errors = 0

vim.bo.shiftwidth = 4
vim.bo.softtabstop = 4

-- TODO: figure out what we want for makeprg here
vim.bo.makeprg = "autopep8"
vim.bo.suffixesadd = ".py"

local python_dir_markers = { "pyprojec.toml", "setup.py", "setup.cfg", ".git" }
local disable_auto_format_files = { ".pynoautoformat", ".pydisableautoformat", ".pydisableformat" }

-- TODO: read project root dir in order to determine which of these to run
local function format_file(opt)
  local bufnr = opt.bufnr
  local project_root = vim.fs.root(vim.fn.expand("%"), python_dir_markers)
  if #vim.fs.find(disable_auto_format_files, { path = project_root, type = "file", limit = 1 }) == 0 then
    local has_formatted = false
    local conform = require("conform")

    -- respect local formatting packages first
    if is_executable("isort") == 1 then
      conform.format({ formatters = { "isort" }, bufnr = bufnr })
    end
    if is_executable("black") == 1 then
      conform.format({ formatters = { "black" }, bufnr = bufnr })
      has_formatted = true
    end

    -- default to ruff if project does not use black
    if not has_formatted then
      conform.format({ formatters = { "ruff" }, bufnr = bufnr })
    end
  end
end

local function lint_file()
  -- using ruff_lsp instead, so not requred here
  local linters = { "mypy", "flake8", "pylint" }
  for _, linter in ipairs(linters) do
    if is_executable(linter) == 1 then
      require("lint").try_lint(linter)
    end
  end
end

vim.api.nvim_create_autocmd("BufWritePre", {
  callback = function(opt)
    format_file(opt)
  end,
  buffer = bufnr,
  group = vim.api.nvim_create_augroup("pyformat:" .. bufnr, {}),
})

vim.api.nvim_create_autocmd({ "BufWritePost" }, {
  callback = function()
    lint_file()
  end,
  buffer = bufnr,
  group = vim.api.nvim_create_augroup("pylint:" .. bufnr, {}),
})

-- set up python test config
require("plugins.test.python").setup()

vim.api.nvim_create_autocmd({ "BufReadPre" }, {
  callback = function()
    if vim.fn.executable("pytest") then
      require("dap-python").test_runner = "pytest"
    else
      require("dap-python").test_runner = "unittest"
    end
  end,
  buffer = bufnr,
  group = vim.api.nvim_create_augroup("set-python-test-runner" .. bufnr, {}),
})
