return {
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    init = function()
      local ensure_installed = { "jsonls", "lua_ls", "pyright", "ts_ls" }
      require("plugins.lsp.config").setup()
    end,
    dependencies = {
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
