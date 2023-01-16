local M = {
  "projekt0n/github-nvim-theme",
  lazy = true,
  version = "*",
}

function M.config()
  require("github-theme").setup({
    theme_style = "dimmed",
    function_style = "italic",
    sidebars = { "qf", "terminal", "packer" },
    dark_sidebar = true,
    dark_float = true,
  })
end

return M
