return {
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      {
        "SmiteshP/nvim-navbuddy",
        dependencies = {
          "SmiteshP/nvim-navic",
          "MunifTanjim/nui.nvim",
        },
        opts = { lsp = { auto_attach = true } },
      },
    },
    config = function()
      local ensure_installed = { "jsonls", "lua_ls", "pyright", "tsserver" }
      require("mason").setup({ providers = { "mason.providers.client", "mason.providers.registry-api" } })
      require("mason-lspconfig").setup({ ensure_installed = ensure_installed })
      require("plugins.lsp.config").setup()
    end,
  },
  {
    "kosayoda/nvim-lightbulb",
    enabled = false,
    opts = {
      autocmd = { enabled = true },
    },
  },
}
