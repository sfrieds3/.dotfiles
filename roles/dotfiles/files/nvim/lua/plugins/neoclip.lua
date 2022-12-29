local M = {
  "AckslD/nvim-neoclip.lua",
  dependencies = {
    { "tami5/sqlite.lua", module = "sqlite" },
  },
}

function M.config()
  require("neoclip").setup({
    enable_macro_history = true,
    enable_persistent_history = true,
    continuous_sync = true,
  })
end

return M
