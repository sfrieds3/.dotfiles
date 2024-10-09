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

local function store_fixtures(fixtures)
  Path:new(full_path):write(vim.json.encode(fixtures), "w")
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

local function ts_query(bufnr, current_pos)
  local lang = "python"
  local parser = vim.treesitter.get_parser(bufnr, lang)
  assert(parser ~= nil, "parser should not be nil")
  local tree = parser:parse()
  assert(tree ~= nil, "tree should not be nil")
  local root = tree[1]:root()
  local query = vim.treesitter.query.parse("python", ts_query_text)

  local current_row, _ = unpack(current_pos)

  local offset = 0
  local max_offset = 10
  local nodes = {}
  while true do
    local start_row = math.max(current_row - offset, 0)
    local end_row = (current_row + offset)

    for id, node in query:iter_captures(root, 0, start_row, end_row) do
      local name = query.captures[id]
      local type = node:type()
      table.insert(nodes, { node = node, name = name, type = type })
    end

    offset = offset + 1
    if #nodes > 0 or offset > max_offset then
      break
    end
  end

  local funcs = {}
  for _, node in ipairs(nodes) do
    table.insert(funcs, vim.treesitter.get_node_text(node.node, bufnr))
  end

  return funcs
end

local function goto_fixture(bufnr, cursor_pos)
  print(string.format("Get fixture for bufnr: %s cursor pos: %s", bufnr, cursor_pos))
  local funcs = ts_query(bufnr, cursor_pos)
  P(funcs)
end

local function store_pytext_fixtures(output_lines)
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

  store_fixtures(fixtures)
end

local function refresh_pytest_fixture_cache()
  local result = {}
  Job:new({
    command = "pytest",
    args = { "--fixtures-per-test" },
    on_exit = function(j, _)
      result = j:result()
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

local function maybe_refresh_pytest_fixture_cache(buf_file)
  if get_project_root(file) and is_python(buf_file) and has_pytest() then
    refresh_pytest_fixture_cache()
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
