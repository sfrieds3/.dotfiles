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

--- Read project fixtures from cache
---@param file_path Path path to project fixture cache file
---@return table fixtures
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

--- Get the parent test function for the node under cursor
---@return string function name
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

--- Convert `target_path` absolute path to a path relative to `base_dir`
---@param base_dir string base directory
---@param target_path string directory
local function get_relative_path(base_dir, target_path)
  local base_abs = vim.fn.fnamemodify(base_dir, ":p")
  local target_abs = vim.fn.fnamemodify(target_path, ":p")

  if target_abs:sub(1, #base_abs) == base_abs then
    return target_abs:sub(#base_abs + 1)
  else
    return target_abs
  end
end

--- Get details about the test under cursor
---@return string, string, string[] relative file name, function name, function args
local function get_current_test_info()
  local function_name = get_parent_test_function()
  local test_file = vim.fn.expand("%")
  local project_root = get_project_root(test_file)
  assert(project_root, "project root should not be nil")
  local relative_file_name = get_relative_path(project_root, test_file)

  -- Query the arguments of the function
  local query_string = [[
    (function_definition
      name: (identifier) @name
      parameters: (parameters (identifier) @args))
  ]]

  local lang = "python"
  local parser = vim.treesitter.get_parser(0, lang)
  assert(parser, "parser should not be nil")

  local query = vim.treesitter.query.parse(lang, query_string)
  local root_node = parser:parse()[1]

  local function_args = {}
  for _, match, _ in query:iter_matches(root_node:root(), 0, 0, -1, { all = true }) do
    local name = vim.treesitter.get_node_text(match[1][1], 0)

    if name == function_name then
      local arg = vim.treesitter.get_node_text(match[2][1], 0)
      table.insert(function_args, arg)
    end
  end

  return relative_file_name, function_name, function_args
end

--- Store pytest fixtures in cache dir
---@param project_hash string filename for project fixture cache
---@param output_lines string[] pytest command result to parse
local function parse_and_store_project_fixtures(project_hash, output_lines)
  local fixtures = setmetatable({}, {
    __index = function(tbl, key)
      tbl[key] = {}
      return tbl[key]
    end,
  })

  local current_test_name = nil
  local current_test_file_path = nil
  local last_line_was_test_heading = false

  for _, line in ipairs(output_lines) do
    if last_line_was_test_heading then
      current_test_file_path = line:match("%((.-):")
      assert(current_test_name, "current test name is nil")
      fixtures[current_test_file_path][current_test_name] = {}
      last_line_was_test_heading = false
    else
      local test_match = line:match("fixtures used by ([%w_]+)")
      if test_match then
        current_test_name = test_match
        current_test_file_path = nil
        last_line_was_test_heading = true
      elseif current_test_name then
        local fixture_name, file_path, line_number = line:match("([%w_]+)%s*%-%-%s*([%w%p]+):(%d+)")
        if fixture_name and file_path then
          fixtures[current_test_file_path][current_test_name][fixture_name] = {
            file_path = file_path,
            line_number = line_number,
          }
        end
      end
    end
  end

  store_fixtures(project_hash, fixtures)
end

--- Kick off a `Job` to refresh the pytest fixture cache for this project
---@param project_hash string unique hash for project, used as filename for cache
local function refresh_pytest_fixture_cache(project_hash)
  local result = {}
  Job:new({
    command = "pytest",
    args = { "--fixtures-per-test" },
    on_exit = function(j, _)
      result = j:result()
      parse_and_store_project_fixtures(project_hash, result)
    end,
  }):start()
end

--- Determine if the a given filename is of python ft
---@param filename string filename
---@return boolean
local function is_python(filename)
  local buf = vim.fn.bufadd(vim.fn.expand(filename))
  vim.fn.bufload(buf)
  local filetype = vim.bo[buf].filetype
  return filetype == "python"
end

--- Determine if pytest is an executable on PATH
---@return boolean
local function has_pytest()
  return vim.fn.executable("pytest") == 1
end

--- Additional predicate check to determine if we should refesh fixture cache
---@param project_hash string unique hash for project
---@return boolean
local function should_refresh_fixtures(project_hash)
  _ = project_hash
  -- TODO: need to add a delay here
  return false
end

--- Generate a unique hash for the project, based on `project_root`
---@param project_root string fully qualified project root
---@return string unique hash
local function generate_project_hash(project_root)
  local project_hash = vim.fn.sha256(project_root)
  return project_hash
end

--- Get the current project root and corresponding hash
---@param buf_file string? file to determine project root of
---@return string, string project_root and hash
local function get_current_project_and_hash(buf_file)
  buf_file = buf_file or vim.fn.expand("%")
  local project_root = get_project_root(buf_file)
  assert(project_root, "project root should not be nil")
  local hash = generate_project_hash(project_root)
  return project_root, hash
end

--- Parse fixtures for an individual test
---@param test_file_name string file name of test
---@param test_name string of test
---@return table[string] fixture details
local function parse_fixtures_for_test(test_file_name, test_name)
  local _, project_hash = get_current_project_and_hash()
  local project_fixture_file_path = get_storage_path_for_project(project_hash)
  local fixtures = get_fixtures(project_fixture_file_path)
  local function_fixtures = fixtures[test_file_name][test_name]
  return function_fixtures
end

--- Open a file at a specified line number
---@param file_path string file path to open
---@param line_number integer line number to jump to
local function open_file_at_line(file_path, line_number)
  vim.cmd("edit " .. file_path)
  vim.api.nvim_win_set_cursor(0, { line_number, 0 })
end

--- Find fixtures associated with test under cursor and prompt to go to them
local function goto_fixture()
  local test_file_name, test_name, test_args = get_current_test_info()
  local function_fixtures = parse_fixtures_for_test(test_file_name, test_name)
  if function_fixtures == nil then
    print("No fixtures found!")
    return
  end
  local fixtures = {}
  for fixture, _ in pairs(function_fixtures) do
    table.insert(fixtures, fixture)
  end

  vim.ui.select(fixtures, {
    prompt = string.format("Go to fixture for test %s: ", test_name),
    format_item = function(item)
      return item
    end,
  }, function(fixture)
    if fixture == nil then
      return
    end

    local fixture_info = function_fixtures[fixture]
    local fixture_line_number = tonumber(fixture_info.line_number) or 0
    -- TODO: should add to tagstack (and make this configurable)
    open_file_at_line(fixture_info.file_path, fixture_line_number)
  end)
end

local function maybe_refresh_pytest_fixture_cache(buf_file, opts)
  opts = opts or {}
  local force = opts.force or false
  local project_root, project_hash = get_current_project_and_hash(buf_file)

  if force or (project_root and is_python(buf_file) and has_pytest() and should_refresh_fixtures(project_hash)) then
    refresh_pytest_fixture_cache(project_hash)
  end
end

vim.api.nvim_create_autocmd("FileType", {
  pattern = "python",
  group = vim.api.nvim_create_augroup("pytest_fixtures:user-commands", { clear = true }),
  callback = function()
    vim.api.nvim_create_user_command("PytestFixturesRefresh", function()
      print("Refreshing for " .. vim.fn.expand("%:p"))
      maybe_refresh_pytest_fixture_cache(vim.fn.expand("%"), { force = true })
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
  goto_fixture()
end, { desc = "PytestFixtures Go To Fixture" })
