local M = {}

function M.setup()
  local dap = require("dap")
  local dap_python = require("dap-python")
  local actions = require("telescope.actions")
  local state = require("telescope.actions.state")
  local config = require("telescope.config")
  local pickers = require("telescope.pickers")
  local finders = require("telescope.finders")

  -- default python
  -- table.insert(dap.configurations.python, {
  --   name = "Pytest: Current File",
  --   type = "python",
  --   request = "launch",
  --   module = "pytest",
  --   args = {
  --     "${file}",
  --     "-sv",
  --     "--log-cli-level=INFO",
  --     "--log-file=test_out.log",
  --   },
  --   console = "integratedTerminal",
  -- })

  -- django configuration
  table.insert(dap.configurations.python, {
    type = "python",
    request = "launch",
    name = "django",
    program = function()
      return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/manage.py", "file")
    end,
    args = { "test", "--keepdb" },
  })

  local function load_vscode_launch_config()
    local launchjs_file = "N/A"
    vim.ui.input(
      { prompt = "Path to vscode launch.json file: ", defualt = vim.fn.getcwd() .. "/.vscode/launch.json" },
      function(input)
        require("dap.ext.vscode").load_launchjs(input)
        launchjs_file = input
      end
    )
    print("Loaded vscode launch.json file: " .. launchjs_file)
  end

  local function set_python_test_runner(opts)
    opts = opts or {}
    pickers
      .new(opts, {
        prompt_title = "Choose Python test runner:",
        finder = finders.new_table({
          results = { "django", "unittest", "pytest" },
        }),
        sorter = config.values.generic_sorter(opts),
        attach_mappings = function(prompt_bufnr, _)
          actions.select_default:replace(function()
            actions.close(prompt_bufnr)
            local selection = state.get_selected_entry()
            require("dap-python").test_runner = selection.value
            print("dap-python test_runner: " .. require("dap-python").test_runner)
          end)
          return true
        end,
      })
      :find()
  end

  require("dap-python").test_runner = "pytest"

  local function set_django_settings_module_env()
    local prev_django_settings_module = vim.env.DJANGO_SETTINGS_MODULE or ""
    vim.ui.input(
      { prompt = "Path to DJANGO_SETTINGS_MODULE: ", defualt = vim.env.DJANGO_SETTINGS_MODULE },
      function(input)
        vim.env.DJANGO_SETTINGS_MODULE = input
      end
    )
    print(
      "\nUpdated $DJANGO_SETTINGS_MODULE from "
        .. prev_django_settings_module
        .. " to "
        .. vim.env.DJANGO_SETTINGS_MODULE
    )
  end

  local just_my_code = true
  local function toggle_just_my_code()
    just_my_code = not just_my_code
    print("justMyCode is", just_my_code and "Enabled" or "Disabled")
  end

  -- stylua: ignore start
  vim.keymap.set("n", "<localleader>dpt", set_python_test_runner, { desc = "Set Dap Python Test Runner" })
  ---@diagnostic disable-next-line: missing-fields
  vim.keymap.set("n", "<M-d>", function() dap_python.test_method({ config = { justMyCode = just_my_code } }) end, { desc = "dap-python: debug function" })
  ---@diagnostic disable-next-line: missing-fields
  vim.keymap.set("n", "<localleader>dtf", function() dap_python.test_method({ config = { justMyCode = just_my_code } }) end, { desc = "dap-python: debug function" })
  vim.keymap.set("n", "<localleader>dtF", function() dap_python.test_method() end, { desc = "dap-python: debug function [project code only]" })
  ---@diagnostic disable-next-line: missing-fields
  vim.keymap.set("n", "<localleader>dtc", function() dap_python.test_class({ config = { justMyCode = false } }) end, { desc = "dap-python: debug class" })
  vim.keymap.set("n", "<localleader>dtC", function() dap_python.test_class() end, { desc = "dap-python: test class [project code only]" })
  -- stylua: ignore end

  vim.api.nvim_create_user_command("TogglePythonJustMyCode", toggle_just_my_code, { nargs = 0 })
  vim.api.nvim_create_user_command("SetPythonTestRunner", set_python_test_runner, { nargs = 0 })
  vim.api.nvim_create_user_command("SetPythonDjangoSettingsModule", set_django_settings_module_env, { nargs = 0 })
  vim.api.nvim_create_user_command("SetPythonLoadVsCodeLaunchJson", load_vscode_launch_config, { nargs = 0 })
end

return M
