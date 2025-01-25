local is_executable = vim.fn.executable

return {
  {
    "danymat/neogen",
    keys = {
      {
        "<leader>cc",
        function()
          require("neogen").generate({ type = "any" })
        end,
        desc = "Neogen Generate Any",
      },
      {
        "<leader>cC",
        function()
          require("neogen").generate({ type = "class" })
        end,
        desc = "Neogen Generate Class",
      },
      {
        "<leader>cF",
        function()
          require("neogen").generate({ type = "func" })
        end,
        desc = "Neogen Generate Function",
      },
      {
        "<leader>cT",
        function()
          require("neogen").generate({ type = "type" })
        end,
        desc = "Neogen Generate Function",
      },
      {
        "<leader>cL",
        function()
          require("neogen").generate({ type = "file" })
        end,
        desc = "Neogen Generate Function",
      },
    },
    opts = {
      snippet_engine = "nvim",
      languages = {
        python = {
          template = {
            annotation_convention = "reST",
          },
        },
      },
    },
  },
}
