return {
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      local ensure_installed = { "jsonls", "lua_ls", "pyright", "tsserver" }
      require("mason-lspconfig").setup({ ensure_installed = ensure_installed })
      require("plugins.lsp.config").setup()
    end,
    dependencies = {
      {
        "williamboman/mason.nvim",
        cmd = { "Mason" },
        opts = {
          providers = { "mason.providers.client", "mason.providers.registry-api" },
        },
        dependencies = {
          "williamboman/mason-lspconfig.nvim",
          "nvim-telescope/telescope.nvim",
        },
      },
      "b0o/SchemaStore.nvim",
      {
        "linrongbin16/lsp-progress.nvim",
        event = "LspAttach",
        opts = {},
      },
      {
        "SmiteshP/nvim-navic",
        event = "LspAttach",
        config = function()
          vim.g.navic_silence = true
          require("nvim-navic").setup({ separator = " ", highlight = true, depth_limit = 5 })
        end,
      },
    },
  },
}
