return {
  {
    "sourcegraph/sg.nvim",
    event = "InsertEnter",
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = {},
  },
  {
    {
      "Exafunction/codeium.nvim",
      enabled = false,
      dependencies = {
        "nvim-lua/plenary.nvim",
        "hrsh7th/nvim-cmp",
      },
      opts = {},
    },
  },
}