local is_executable = vim.fn.executable

return {
  {
    "norcalli/nvim-colorizer.lua",
    ft = { "html", "css", "javascript", "vim", "eruby" },
    cmd = { "ColorizerToggle", "ColorizerAttachToBuffer" },

    config = function()
      require("colorizer").setup({
        default_options = {
          RGB = false,
        },
      })
    end,

    keys = {
      {
        "<leader>tc",
        "<Cmd>ColorizerToggle<CR>",
        desc = "[C]olorizer toggle",
      },
    },
  },
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
