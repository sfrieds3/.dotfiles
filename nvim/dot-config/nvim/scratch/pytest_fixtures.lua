local Job = require("plenary.job")

-- Modify process_pytest_output to take input as a table
local function process_pytest_output(output_lines)
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

  P(fixtures)
end

local function get_pytest_output()
  local result = {}
  Job:new({
    command = "pytest",
    args = { "--fixtures-per-test" },
    on_exit = function(j, return_val)
      result = j:result() -- Capture the output in a table
      process_pytest_output(result)
    end,
  }):start()
end

local function test()
  get_pytest_output()
end

test()

vim.api.nvim_create_user_command("PytestFixtures", get_pytest_output, {})
