local Path = require("plenary.path")
local Job = require("plenary.job")
local TSUtils = require("nvim-treesitter.ts_utils")

local project_markers = { ".git", "pyproject.toml", "setup.py", "setup.cfg" }

--- Get project root for a given file based on configured project_markers
---@param file string filename for which to find root directory
---@return string? root directory for `file`
local function get_project_root(file)
  return vim.fs.root(vim.fn.expand(file), project_markers)
end

local data_path = string.format("%s/pytest_fixtures.nvim", vim.fn.stdpath("data"))
local data_path_exists = false
--- Ensure configured data path exists
local function ensure_data_path_exists()
  if data_path_exists then
    return
  end

  local path = Path:new(data_path)
  if not path:exists() then
    path:mkdir()
  end
  data_path_exists = true
end

ensure_data_path_exists()

--- Get a `Path` object to store pytest fixture data for a project
---@param project_hash string unique hash for project
---@return Path path object to store project data
local function get_storage_path_for_project(project_hash)
  local full_path = string.format("%s/%s.json", data_path, project_hash)
  return Path:new(full_path)
end

--- Store fixture to unique `project_hash` location for project
---@param project_hash string unique hash for project
---@param fixtures table pytest fixtures for a given file  -- TODO: better typing for this
local function store_fixtures(project_hash, fixtures)
  local path = get_storage_path_for_project(project_hash)
  path:write(vim.json.encode(fixtures), "w")
end

local function get_fixtures(file_path)
  local path = Path:new(file_path)
  local raw_fixtures = path:read()

  local fixtures = vim.json.decode(raw_fixtures)
  return fixtures
end

local ts_query_text = [[
(function_definition
  name: (identifier) @function)
  ]]

local function get_test_args()
  local function_at_cursor = TSUtils.get_node_at_cursor()

  -- Traverse up to find the function definition node
  while function_at_cursor and function_at_cursor:type() ~= "function_definition" do
    function_at_cursor = function_at_cursor:parent()
  end

  if not function_at_cursor then
    return
  end

  local function_name = vim.treesitter.get_node_text(function_at_cursor:field("name")[1], 0)

  -- Query the arguments of the function
  local query_string = [[
    (function_definition
      name: (identifier) @name
      parameters: (parameters (identifier) @args))
  ]]

  local lang = "python"
  local parser = vim.treesitter.get_parser(0, lang)
  if parser == nil then
    return
  end
  local tree = parser:parse()[1]
  local root = tree:root()
  local query = vim.treesitter.query.parse(lang, query_string)

  local args = {}
  for id, node, _ in query:iter_captures(root, 0) do
    local capture_name = query.captures[id]
    if capture_name == "args" then
      table.insert(args, vim.treesitter.get_node_text(node, 0))
    end
  end

  return function_name, args
end

local function goto_fixture(bufnr, cursor_pos)
  print(string.format("Get fixture for bufnr: %s cursor pos: %s", bufnr, cursor_pos))
  local func, args = get_test_args()
  print(func, vim.inspect(args))
end

local function store_pytext_fixtures(project_hash, output_lines)
  local fixtures = {}
  local current_test = nil

  for _, line in ipairs(output_lines) do
    local test_match = line:match("fixtures used by ([%w_%[%]-]+)")
    if test_match then
      current_test = test_match
      fixtures[current_test] = {}
    elseif current_test then
      local fixture_name, file_path = line:match("^(.-)%s*%-%-%s*(.+)$")
      if fixture_name and file_path then
        fixtures[current_test][fixture_name] = file_path
      end
    end
  end

  store_fixtures(project_hash, fixtures)
end

local function refresh_pytest_fixture_cache(project_hash)
  local result = {}
  Job:new({
    command = "pytest",
    args = { "--fixtures-per-test" },
    on_exit = function(j, _)
      result = j:result()
      store_pytext_fixtures(project_hash, result)
    end,
  }):start()
end

local function is_python(filename)
  local buf = vim.fn.bufadd(vim.fn.expand(filename))
  vim.fn.bufload(buf)

  local filetype = vim.bo[buf].filetype

  return filetype == "python"
end

local function has_pytest()
  return vim.fn.executable("pytest") == 1
end

local function should_refresh_fixtures(project_hash)
  _ = project_hash
  return true
end

local function maybe_refresh_pytest_fixture_cache(buf_file)
  local project_root = get_project_root(buf_file)
  if project_root == nil then
    return
  end

  local project_hash = vim.fn.sha256(project_root)
  if project_root and is_python(buf_file) and has_pytest() and should_refresh_fixtures(project_hash) then
    refresh_pytest_fixture_cache(project_hash)
  end
end

vim.api.nvim_create_autocmd("FileType", {
  pattern = "python",
  group = vim.api.nvim_create_augroup("pytest_fixtures:user-commands", { clear = true }),
  callback = function()
    vim.api.nvim_create_user_command("PytestFixturesRefresh", function()
      print("refreshing for " .. vim.fn.expand("%:p"))
      maybe_refresh_pytest_fixture_cache(vim.fn.expand("%"))
    end, {})
  end,
})

vim.api.nvim_create_autocmd({ "BufEnter", "BufWinEnter", "BufWritePost" }, {
  group = vim.api.nvim_create_augroup("pytest_fixtures:refresh", { clear = true }),
  pattern = { "*.py", "*.pyi" },
  callback = function(ev)
    maybe_refresh_pytest_fixture_cache(ev.file)
  end,
})

vim.keymap.set("n", "<localleader>]", function()
  goto_fixture(vim.api.nvim_get_current_buf(), vim.api.nvim_win_get_cursor(0))
end, { desc = "PytestFixtures Go To Fixture" })
