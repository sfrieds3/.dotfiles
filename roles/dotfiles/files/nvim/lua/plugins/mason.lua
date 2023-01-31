local M = {
  "williamboman/mason.nvim",
  cmd = "Mason",
  dependencies = {
    "williamboman/mason-lspconfig.nvim",
  },
}

function M.config()
  require("mason").setup({ providers = { "mason.providers.client", "mason.providers.registry-api" } })
  require("mason-lpsconfig").setup()
end

return M