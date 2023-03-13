return {
  "mfussenegger/nvim-dap",
  config = function()
    require("plugins.dap.config").setup()
  end,
  event = "VeryLazy",
  dependencies = {
    {
      "theHamsta/nvim-dap-virtual-text",
      config = function()
        require("plugins.dap.dap_virtual_text").setup()
      end,
    },
    { "nvim-telescope/telescope-dap.nvim", dependencies = { "nvim-telescope/telescope.nvim" } },
    {
      "mfussenegger/nvim-dap-python",
      ft = "python",
      config = function()
        require("plugins.dap.dap_python").setup()
      end,
    },
    {
      "rcarriga/nvim-dap-ui",
      config = function()
        require("plugins.dap.dap_ui").setup()
      end,
    },
  },
}
