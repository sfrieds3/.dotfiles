return {
  {
    "neovim/nvim-lspconfig",
    config = function()
      require("plugins.lsp.lsp").setup()
    end,
    dependencies = {
      { "nvim-lua/lsp-status.nvim" },
    },
  },
  {
    "jose-elias-alvarez/null-ls.nvim",
    config = function()
      require("plugins.lsp.null_ls").setup()
    end,
  },
}
