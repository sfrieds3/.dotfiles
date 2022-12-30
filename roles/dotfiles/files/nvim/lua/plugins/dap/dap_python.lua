local M = {}

function M.setup()
  local dap = require("dap")
  local dp = require("dap-python")

  local actions = require("telescope.actions")
  local actions_state = require("telescope.actions.state")
  local config = require("telescope.config")
  local pickers = require("telescope.pickers")
  local finders = require("telescope.finders")

  dp.setup("~/.venv/venv/bin/python")

  -- django configuration
  table.insert(dap.configurations.python, {
    type = "python",
    request = "launch",
    name = "Django",
    program = function()
      return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/manage.py", "file")
    end,
    args = { "runserver", "--noreload" },
  })

  local set_python_test_runner = function(opts)
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
            local selection = actions_state.get_selected_entry()
            require("dap-python").test_runner = selection.value
            print("dap-python test_runner: " .. require("dap-python").test_runner)
          end)
          return true
        end,
      })
      :find()
  end

  local set_django_settings_module_env = function()
    local prev_django_settings_module = vim.env.DJANGO_SETTINGS_MODULE
    vim.ui.input(
      { prompt = "Path to DJANGO_SETTINGS_MODULE: ", defualt = vim.env.DJANGO_SETTINGS_MODULE },
      function(input)
        vim.env.DJANGO_SETTINGS_MODULE = input
      end
    )
    require("notify")(
      string.format(
        "\nUpdated $DJANGO_SETTINGS_MODULE from %s to %s",
        prev_django_settings_module,
        vim.env.DJANGO_SETTINGS_MODULE
      ),
      "info",
      {
        render = "simple",
        title = "Updated $DJANGO_SETTINGS_MODULE",
      }
    )
  end

  vim.keymap.set("n", "\\dpt", set_python_test_runner, { desc = "[d]ap-[p]ython: set python [t]est_runner" })

  vim.api.nvim_create_autocmd({ "BufEnter" }, {
    group = vim.api.nvim_create_augroup("DapPython", { clear = true }),
    callback = function()
      vim.keymap.set("n", "\\tf", function()
        dp.test_method()
      end, { desc = "dap-python: test function" })
      vim.api.nvim_create_user_command("DapPythonTestRunner", set_python_test_runner, { nargs = 0 })
      vim.api.nvim_create_user_command("DapDjangoSettingsModule", set_django_settings_module_env, { nargs = 0 })
    end,
  })
end

return M