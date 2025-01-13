return {
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      local ensure_installed = { "jsonls", "lua_ls", "pyright", "ts_ls" }
      require("mason-lspconfig").setup({ ensure_installed = ensure_installed })
      require("plugins.lsp.config").setup()
    end,
    dependencies = {
      {
        "williamboman/mason.nvim",
        cmd = { "Mason" },
        opts = {
          providers = { "mason.providers.client", "mason.providers.registry-api" },
          PATH = "append",
        },
        dependencies = {
          "williamboman/mason-lspconfig.nvim",
        },
      },
      "b0o/SchemaStore.nvim",
      {
        "linrongbin16/lsp-progress.nvim",
        event = "LspAttach",
        opts = {},
      },
      "saghen/blink.cmp",
    },
  },
}
