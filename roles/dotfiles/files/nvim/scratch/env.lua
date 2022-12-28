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
      render = "minimal",
    }
  )
end

set_django_settings_module_env()
