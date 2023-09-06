local is_executable = vim.fn.executable
local bufnr = vim.api.nvim_get_current_buf()

vim.g.python_highlight_space_errors = 0

vim.bo.shiftwidth = 4
vim.bo.softtabstop = 4

-- TODO: figure out what we want for makeprg here
vim.bo.makeprg = "autopep8"
vim.bo.suffixesadd = ".py"

local function run_cmds(cmds)
  return function()
    for _, cmd in pairs(cmds) do
      vim.cmd(cmd)
    end
    vim.cmd("silent e!")
    if is_executable("ruff") == 1 then
      require("lint").try_lint("ruff")
    end
    if is_executable("mypy") == 1 then
      require("lint").try_lint("mypy")
    end
  end
end

local on_write = {}
-- TODO: do we need these since we use formatter?
if is_executable("isort") == 1 then
  table.insert(on_write, "silent !isort --profile black %")
end
if is_executable("black") == 1 then
  table.insert(on_write, "silent !black -q %")
end

if next(on_write) then
  vim.api.nvim_create_autocmd("BufWritePost", {
    callback = run_cmds(on_write),
    buffer = bufnr,
    group = vim.api.nvim_create_augroup("black:" .. bufnr, {}),
  })
else
  vim.api.nvim_create_autocmd("BufWritePost", {
    callback = function()
      require("lint").try_lint("ruff")
    end,
    buffer = bufnr,
    group = vim.api.nvim_create_augroup("ruff:" .. bufnr, {}),
  })
end
