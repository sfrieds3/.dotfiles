local Path = require("plenary.path")
local Job = require("plenary.job")

local project_markers = { ".git", "pyproject.toml", "setup.py", "setup.cfg" }

local function get_project_root(file)
  return vim.fs.root(vim.fn.expand(file), project_markers)
end

local data_path = string.format("%s/pytest_fixtures.nvim", vim.fn.stdpath("data"))
local data_path_exists = false
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
local file = "~/code/prefect/setup.py"
local project_root = get_project_root(file)
local project_hash = vim.fn.sha256(project_root)
local full_path = string.format("%s/%s.json", data_path, project_hash)
print("data path" .. full_path)

local function store_fixtures(fixtures)
  Path:new(full_path):write(vim.json.encode(fixtures), "w")
  print(string.format("Fixtures for %s stored in %s", get_project_root(file), full_path))
end

local function get_fixtures(file_path)
  local path = Path:new(file_path)
  local raw_fixtures = path:read()

  local fixtures = vim.json.decode(raw_fixtures)
  return fixtures
end

-- Modify process_pytest_output to take input as a table
local function store_pytext_fixtures(output_lines)
  local fixtures = {}
  local current_test = nil

  -- Parsing the input table
  for _, line in ipairs(output_lines) do
    local test_match = line:match("fixtures used by ([%w_%[%]-]+)")
    if test_match then
      current_test = test_match
      fixtures[current_test] = {} -- Initialize the fixture table for the current test
    elseif current_test then
      -- Find fixture lines and capture both the fixture and the filename
      local fixture_name, file_path = line:match("^(.-)%s*%-%-%s*(.+)$")
      if fixture_name and file_path then
        -- Store the fixture name and its file as key-value pairs
        fixtures[current_test][fixture_name] = file_path
      end
    end
  end

  store_fixtures(fixtures)
end

local function refresh_pytest_fixture_cache()
  local result = {}
  Job:new({
    command = "pytest",
    args = { "--fixtures-per-test" },
    on_exit = function(j, _)
      result = j:result() -- Capture the output in a table
      store_pytext_fixtures(result)
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

local function maybe_refresh_pytest_fixture_cache()
  if get_project_root(file) and is_python(file) and has_pytest() then
    refresh_pytest_fixture_cache()
  end
end

local function test()
  refresh_pytest_fixture_cache()
end

-- test()
--
-- P(get_fixtures(full_path))

vim.api.nvim_create_user_command("PytestFixturesRefresh", maybe_refresh_pytest_fixture_cache, {})
