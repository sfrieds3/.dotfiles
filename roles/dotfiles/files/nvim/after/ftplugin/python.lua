local is_executable = vim.fn.executable
local bufnr = vim.api.nvim_get_current_buf()

vim.g.python_highlight_space_errors = 0

vim.bo.shiftwidth = 4
vim.bo.softtabstop = 4

-- TODO: figure out what we want for makeprg here
vim.bo.makeprg = "autopep8"
vim.bo.suffixesadd = ".py"

local function format_file()
  if is_executable("isort") == 1 then
    require("conform").format({ formatters = { "isort" }, bufnr = bufnr })
  end
  if is_executable("black") == 1 then
    require("conform").format({ formatters = { "black" }, bufnr = bufnr })
  end
end
local function lint_file()
  local linters = { "ruff", "mypy", "pydocstyle", "flake8", "pylint" }
  for _, linter in ipairs(linters) do
    if is_executable(linter) == 1 then
      require("lint").try_lint(linter)
    end
  end
end

vim.api.nvim_create_autocmd("BufWritePre", {
  callback = function()
    format_file()
  end,
  buffer = bufnr,
  group = vim.api.nvim_create_augroup("pyformat:" .. bufnr, {}),
})

vim.api.nvim_create_autocmd({ "BufWritePost", "BufEnter", "BufLeave" }, {
  callback = function()
    lint_file()
  end,
  buffer = bufnr,
  group = vim.api.nvim_create_augroup("pylint:" .. bufnr, {}),
})
