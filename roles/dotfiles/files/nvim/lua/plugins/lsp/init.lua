return {
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      "nvim-lua/lsp-status.nvim",
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
    },
  },
  {
    "jose-elias-alvarez/null-ls.nvim",
    config = function()
      local ensure_installed = { "jsonls", "lua_ls", "pyright", "tsserver" }
      require("mason").setup({ providers = { "mason.providers.client", "mason.providers.registry-api" } })
      require("mason-lspconfig").setup({ ensure_installed = ensure_installed })
      require("plugins.lsp.null_ls").setup()
      require("plugins.lsp.lsp").setup()
    end,
  },
}
