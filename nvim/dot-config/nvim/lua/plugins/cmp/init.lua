return {
  {
    "hrsh7th/nvim-cmp",
    enabled = true,
    event = { "InsertEnter", "CmdLineEnter" },
    config = function()
      require("plugins.cmp.config").setup()
    end,
    dependencies = {
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-cmdline",
      "hrsh7th/cmp-nvim-lua",
      "hrsh7th/cmp-path",
      "saadparwaiz1/cmp_luasnip",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-nvim-lsp-signature-help",
      "hrsh7th/cmp-nvim-lsp-document-symbol",
      "onsails/lspkind.nvim",
    },
  },
}
