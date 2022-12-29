local M = {
  "Shatur/neovim-session-manager",
}

function M.config()
  require("session_manager").setup({
    sessions_dir = vim.fn.expand(vim.fn.stdpath("data") .. "/sessions/"),
    autoload_mode = require("session_manager.config").AutoloadMode.Disabled,
    autosave_only_in_session = true,
  })
end

return M
