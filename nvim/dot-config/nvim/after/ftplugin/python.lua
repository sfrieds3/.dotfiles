local is_executable = vim.fn.executable
local bufnr = vim.api.nvim_get_current_buf()

vim.b[bufnr].disable_conform_autoformat = true

vim.opt_local.shiftwidth = 4
vim.opt_local.softtabstop = 4
vim.opt_local.textwidth = 120

-- vim.opt_local.makeprg = "ruff check %"
vim.opt_local.makeprg = "pytest % $*"

-- Add pytest error format
vim.opt.errorformat:append({
  [[\%EE\ \ \ \ \ File\ \"%f\"\\,\ line\ %l]],
  [[\%CE\ \ \ %p^]],
  [[\%ZE\ \ \ %[%^\ ]%\\@=%m]],
  [[\%Afile\ %f\\,\ line\ %l]],
  [[\%+ZE\ %mnot\ found]],
  [[\%CE\ %.%#]],
  [[\%-G_%\\+\ ERROR%.%#\ _%\\+]],
  [[\%A_%\\+\ %o\ _%\\+]],
  [[\%C%f:%l:\ in\ %o]],
  [[\%ZE\ %\\{3}%m]],
  [[\%EImportError%.%#\'%f\'\.]],
  [[\%C%.%#]],
  [[\%+G%[=]%\\+\ %*\\d\ passed%.%#]],
  [[\%-G%[%^E]%.%#]],
  [[\%-G]],
})

vim.opt_local.suffixesadd = ".py"

local python_dir_markers = { "pyprojec.toml", "setup.py", "setup.cfg", ".git" }
local disable_auto_format_files = { ".noautoformat", ".disableformat", ".pynoautoformat" }

local function format_file(opts)
  if vim.g.disable_autoformat or vim.b[opts.buf].disable_autoformat then
    return
  end

  local has_formatted = false
  local ran_isort = false
  local conform = require("conform")

  -- respect local formatting packages first
  if is_executable("isort") == 1 then
    conform.format({ formatters = { "isort" }, bufnr = bufnr })
    ran_isort = true
  end
  if is_executable("black") == 1 then
    conform.format({ formatters = { "black" }, bufnr = bufnr })
    has_formatted = true
  end

  -- default to ruff if project does not use black
  if not ran_isort and not has_formatted then
    conform.format({ formatters = { "ruff_organize_imports" }, bufnr = bufnr })
  end
  if not has_formatted then
    conform.format({ formatters = { "ruff_format" }, bufnr = bufnr })
  end
end

local function lint_file()
  -- using ruff via lsp so not required here
  local linters = { "mypy", "flake8", "pylint" }
  for _, linter in ipairs(linters) do
    if is_executable(linter) == 1 then
      require("lint").try_lint(linter)
    end
  end
end

local function set_python_format_config()
  local project_root = vim.fs.root(vim.fn.expand("%"), python_dir_markers)

  if #vim.fs.find(disable_auto_format_files, { path = project_root, type = "file", limit = 1, upward = true }) ~= 0 then
    vim.b[bufnr].disable_autoformat = true
  end

  if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
    return
  end

  vim.api.nvim_create_autocmd("BufWritePre", {
    callback = function(opts)
      format_file(opts)
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
end
set_python_format_config()
