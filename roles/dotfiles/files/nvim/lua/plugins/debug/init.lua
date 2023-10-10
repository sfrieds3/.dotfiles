return {
  {
    "nvim-neotest/neotest",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter",
      "nvim-neotest/neotest-python",
      "nvim-neotest/neotest-plenary",
    },
    config = function()
      require("neotest").setup({
        adapters = {
          require("neotest-python")({
            dap = { justMyCode = false },
          }),
          require("neotest-plenary"),
        },
      })
    end,
  },
  {
    "folke/neodev.nvim",
    config = function()
      require("neodev").setup({
        library = { plugins = { "neotest" }, types = true },
      })
    end,
  },
  {
    "theHamsta/nvim-dap-virtual-text",
    lazy = true,
    config = function()
      require("plugins.debug.dap_virtual_text").setup()
    end,
  },
  {
    "nvim-telescope/telescope-dap.nvim",
    lazy = true,
    dependencies = { "nvim-telescope/telescope.nvim" },
  },
  {
    "mfussenegger/nvim-dap-python",
    lazy = true,
    ft = "python",
    config = function()
      require("plugins.debug.dap_python").setup()
    end,
  },
  {
    "rcarriga/nvim-dap-ui",
    lazy = true,
    config = function()
      require("plugins.debug.dap_ui").setup()
    end,
  },
  {
    "mfussenegger/nvim-dap",
    config = function()
      require("plugins.debug.config").setup()
    end,
    event = "VeryLazy",
    dependencies = {
      "theHamsta/nvim-dap-virtual-text",
      "nvim-telescope/telescope-dap.nvim",
      "mfussenegger/nvim-dap-python",
      "rcarriga/nvim-dap-ui",
    },
  },
}
