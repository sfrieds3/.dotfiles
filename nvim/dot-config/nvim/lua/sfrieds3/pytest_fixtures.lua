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

local data_path = string.format("%s/pytest_fixtures", vim.fn.stdpath("data"))
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
  name: (identifier) @function
  #match? @function "test_")
  ]]

local function get_parent_test_function()
  local node_at_cursor = TSUtils.get_node_at_cursor()

  while node_at_cursor do
    if node_at_cursor:type() == "function_definition" then
      local function_name_node = node_at_cursor:field("name")[1]
      if function_name_node then
        local function_name = vim.treesitter.get_node_text(function_name_node, 0)
        if function_name:match("^test_") then
          return function_name
        end
      end
    end
    node_at_cursor = node_at_cursor:parent()
  end

  return nil
end

local function get_all_test_args()
  local function_name = get_parent_test_function()

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

  local query = vim.treesitter.query.parse(lang, query_string)
  local root_node = parser:parse()[1]

  local function_args = {}
  for pattern, match, metadata in query:iter_matches(root_node:root(), 0, 0, -1, { all = true }) do
    local name = vim.treesitter.get_node_text(match[1][1], 0)

    if name == function_name then
      local arg = vim.treesitter.get_node_text(match[2][1], 0)
      table.insert(function_args, arg)
    end
  end

  return function_name, function_args
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

local function generate_project_hash(project_root)
  local project_hash = vim.fn.sha256(project_root)
  return project_hash
end

local function get_current_project_and_hash(buf_file)
  buf_file = buf_file or vim.fn.expand("%")
  local project_root = get_project_root(buf_file)
  local hash = generate_project_hash(project_root)
  return project_root, hash
end

local function parse_fixtures_for_test(test_name)
  local _, project_hash = get_current_project_and_hash()
  local project_fixture_file_path = get_storage_path_for_project(project_hash)
  local fixtures = get_fixtures(project_fixture_file_path)
  local function_fixtures = fixtures[test_name]

  print("FUNCTION FIXTURES: ", vim.inspect(function_fixtures))
end

local function goto_fixture(bufnr, cursor_pos)
  local test_name, test_args = get_all_test_args()
  local fixtures = parse_fixtures_for_test(test_name)
  print(test_name, vim.inspect(test_args))
end

local function maybe_refresh_pytest_fixture_cache(buf_file)
  local project_root, project_hash = get_current_project_and_hash(buf_file)

  if project_root and is_python(buf_file) and has_pytest() and should_refresh_fixtures(project_hash) then
    refresh_pytest_fixture_cache(project_hash)
  end
end

vim.api.nvim_create_autocmd("FileType", {
  pattern = "python",
  group = vim.api.nvim_create_augroup("pytest_fixtures:user-commands", { clear = true }),
  callback = function()
    vim.api.nvim_create_user_command("PytestFixturesRefresh", function()
      print("Refreshing for " .. vim.fn.expand("%:p"))
      maybe_refresh_pytest_fixture_cache(vim.fn.expand("%"))
    end, {})
    vim.api.nvim_create_user_command("PytestFixturesProjectCachePath", function()
      local _, project_hash = get_current_project_and_hash()
      local cache = get_storage_path_for_project(project_hash)
      print("Project cache location: ", cache)
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
