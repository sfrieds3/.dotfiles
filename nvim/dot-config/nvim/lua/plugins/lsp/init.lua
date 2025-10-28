return {
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      require("plugins.lsp.config").setup()
    end,
    dependencies = {
      "b0o/SchemaStore.nvim",
      {
        "linrongbin16/lsp-progress.nvim",
        event = "LspAttach",
        opts = {},
      },
    },
  },
}
