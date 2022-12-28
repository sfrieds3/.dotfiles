local actions = require("telescope.actions")
local actions_state = require("telescope.actions.state")
local config = require("telescope.config")
local pickers = require("telescope.pickers")
local finders = require("telescope.finders")

local set_python_test_runner = function(opts)
  opts = opts or {}
  pickers
    .new(opts, {
      prompt_title = "Choose Python test runner:",
      finder = finders.new_table({
        results = { "Django", "unittest", "pytest" },
      }),
      sorter = config.values.generic_sorter(opts),
      attach_mappings = function(prompt_bufnr, _)
        actions.select_default:replace(function()
          actions.close(prompt_bufnr)
          local selection = actions_state.get_selected_entry()
          require("dap-python").test_runner = selection.value
          print("dap-python test_runner: " .. require("dap-python").test_runner)
        end)
        return true
      end,
    })
    :find()
end

set_python_test_runner(require("telescope.themes").get_dropdown())
