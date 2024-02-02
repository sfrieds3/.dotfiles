local set_django_settings_module_env = function()
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

set_django_settings_module_env()
