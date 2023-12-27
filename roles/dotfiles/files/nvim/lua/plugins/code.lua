local is_executable = vim.fn.executable

return {
  {
    "L3MON4D3/LuaSnip",
    event = "InsertEnter",
    dependencies = {
      { "rafamadriz/friendly-snippets" },
    },

    config = function()
      require("luasnip.loaders.from_vscode").lazy_load()
      require("luasnip.loaders.from_lua").lazy_load({ paths = vim.fn.stdpath("data") .. "/snippets_local" })
    end,
  },
  {
    "numToStr/Comment.nvim",
    config = true,
  },
  { "RRethy/nvim-align", cmd = { "Align" } },
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    opts = {},
  },
  {
    "Vigemus/iron.nvim",
    config = function()
      local iron = require("iron.core")
      iron.setup({
        config = {
          scratch_repl = true,
          repl_definition = {
            sh = {
              command = { "fish" },
            },
          },
          -- repl_open_cmd = require("iron.view").right(function()
          --   return math.floor(vim.o.columns * 0.4)
          -- end),
          repl_open_cmd = "vsplit",
        },
        keymaps = {
          send_motion = "\\sc",
          visual_send = "\\sc",
          send_file = "\\sf",
          send_line = "\\sl",
          send_until_cursor = "\\su",
          send_mark = "\\sm",
          mark_motion = "\\mc",
          mark_visual = "\\mc",
          remove_mark = "\\md",
          cr = "\\s<cr>",
          interrupt = "\\s<space>",
          exit = "\\sq",
          clear = "\\cl",
        },
        highlight = {
          italic = true,
        },
        ignore_blank_lines = true,
      })

      vim.keymap.set("n", "\\i", "<Cmd>IronRepl<CR>")
      vim.keymap.set("n", "\\I", "<Cmd>IronReplHere<CR>")
    end,
  },
  {
    "kevinhwang91/nvim-ufo",
    dependencies = {
      "kevinhwang91/promise-async",
    },
    config = function()
      require("ufo").setup({
        provider_selector = function(bufnr, filetype, buftype)
          return { "treesitter", "indent" }
        end,
      })
    end,
  },
  {
    "FredeEB/tardis.nvim",
    opts = {
      keymap = {
        ["next"] = "<C-j>", -- next entry in log (older)
        ["prev"] = "<C-k>", -- previous entry in log (newer)
        ["quit"] = "q", -- quit all
        ["revision_message"] = "m", -- show revision message for current revision
      },
      initial_revisions = 10, -- initial revisions to create buffers for
      max_revisions = 256, -- max number of revisions to load
    },
  },
  {
    "cshuaimin/ssr.nvim",
    keys = {
      {
        "<localleader>sr",
        function()
          require("ssr").open()
        end,
        mode = { "n", "x" },
        desc = "SSR",
      },
    },
  },
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
        "_C",
        "<Cmd>ColorizerToggle<CR>",
        desc = "[C]olorizer toggle",
      },
    },
  },
}
