return {
  {
    "sourcegraph/sg.nvim",
    event = "InsertEnter",
    enabled = false,
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = {},
  },
  {
    "Exafunction/codeium.nvim",
    enabled = false,
    dependencies = {
      "nvim-lua/plenary.nvim",
    },
    opts = {},
  },
}
