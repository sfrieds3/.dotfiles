local M = {}
local theme = require('telescope.themes')

M.repo_files = function()
  require('telescope.builtin').git_files(theme.get_ivy())
end

M.project_files = function()
  require('telescope.builtin').find_files(theme.get_ivy())
end

return M

