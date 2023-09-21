return {
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      "nvim-lua/lsp-status.nvim",
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
      {
        "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
        config = function()
          require("lsp_lines").setup()
          vim.keymap.set("", "<Leader>l", require("lsp_lines").toggle, { desc = "Toggle lsp_lines" })
        end,
      },
    },
    config = function()
      local ensure_installed = { "jsonls", "lua_ls", "pyright", "tsserver" }
      require("mason").setup({ providers = { "mason.providers.client", "mason.providers.registry-api" } })
      require("mason-lspconfig").setup({ ensure_installed = ensure_installed })
      require("plugins.lsp.lsp").setup()
    end,
  },
}
