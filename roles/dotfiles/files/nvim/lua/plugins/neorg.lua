local M = {
  "nvim-neorg/neorg",
  build = ":Neorg sync-parsers",
  dependencies = { "nvim-lua/plenary.nvim" },
}

function M.config()
  require("neorg").setup({
    load = {
      ["core.defaults"] = {},
      ["core.concealer"] = {},
      ["core.dirman"] = {
        config = {
          workspaces = {
            wiki = "~/wiki",
          },
          default_workspace = "wiki",
        },
      },
    },
  })

  vim.wo.foldlevel = 99
  vim.wo.conceallevel = 2
end

return M
