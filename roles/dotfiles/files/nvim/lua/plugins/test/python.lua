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
  table.insert(dap.configurations.python, {
    name = "Pytest: Current File",
    type = "python",
    request = "launch",
    module = "pytest",
    args = {
      "${file}",
      "-sv",
      "--log-cli-level=INFO",
      "--log-file=test_out.log",
    },
    console = "integratedTerminal",
  })

  -- django configuration
  table.insert(dap.configurations.python, {
    type = "python",
    request = "launch",
    name = "django",
    program = function()
      return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/manage.py", "file")
    end,
    args = { "runserver", "--noreload", "--insecure" },
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

  local function set_django_settings_module_env()
    local prev_django_settings_module = vim.env.DJANGO_SETTINGS_MODULE
    vim.ui.input(
      { prompt = "Path to DJANGO_SETTINGS_MODULE: ", defualt = vim.env.DJANGO_SETTINGS_MODULE },
      function(input)
        vim.env.DJANGO_SETTINGS_MODULE = input
      end
    )
    print(
      "Updated $DJANGO_SETTINGS_MODULE from " .. prev_django_settings_module .. " to " .. vim.env.DJANGO_SETTINGS_MODULE
    )
  end

  -- stylua: ignore
  vim.keymap.set("n", "\\dpt", set_python_test_runner, { desc = "Set Dap Python Test Runner" })
  vim.keymap.set("n", "\\tf", function()
    dap_python.test_method()
  end, { desc = "dap-python: test function" })
  vim.keymap.set("n", "\\tc", function()
    dap_python.test_class()
  end, { desc = "dap-python: test class" })

  vim.api.nvim_create_user_command("SetPythonTestRunner", set_python_test_runner, { nargs = 0 })
  vim.api.nvim_create_user_command("SetPythonTestRunner", set_python_test_runner, { nargs = 0 })
  vim.api.nvim_create_user_command("SetPythonDjangoSettingsModule", set_django_settings_module_env, { nargs = 0 })
  vim.api.nvim_create_user_command("SetPythonLoadVsCodeLaunchJson", load_vscode_launch_config, { nargs = 0 })
end

return M
