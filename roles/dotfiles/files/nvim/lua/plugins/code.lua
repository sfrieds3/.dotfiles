local is_executable = vim.fn.executable

return {
  {
    "L3MON4D3/LuaSnip",
    event = "InsertEnter",
    build = "make install_jsregexp",
    dependencies = {
      { "rafamadriz/friendly-snippets" },
    },

    config = function()
      require("luasnip.loaders.from_vscode").lazy_load()
      require("luasnip.loaders.from_lua").lazy_load({ paths = { vim.fn.stdpath("data") .. "/snippets_local" } })
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
    config = function()
      require("nvim-autopairs").setup({})
      local npairs = require("nvim-autopairs")
      local Rule = require("nvim-autopairs.rule")
      local cond = require("nvim-autopairs.conds")
      local function rule(a1, ins, a2, lang)
        npairs.add_rule(Rule(ins, ins, lang)
          :with_pair(function(opts)
            return a1 .. a2 == opts.line:sub(opts.col - #a1, opts.col + #a2 - 1)
          end)
          :with_move(cond.none())
          :with_cr(cond.none())
          :with_del(function(opts)
            local col = vim.api.nvim_win_get_cursor(0)[2]
            return a1 .. ins .. ins .. a2 == opts.line:sub(col - #a1 - #ins + 1, col + #ins + #a2) -- insert only works for #ins == 1 anyway
          end))
      end

      rule("(", "*", ")", "ocaml")
      rule("(*", " ", "*)", "ocaml")
      rule("(", " ", ")")
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
  {
    "echasnovski/mini.ai",
    -- keys = {
    --   { "a", mode = { "x", "o" } },
    --   { "i", mode = { "x", "o" } },
    -- },
    event = "VeryLazy",
    opts = function()
      local ai = require("mini.ai")
      return {
        n_lines = 500,
        custom_textobjects = {
          o = ai.gen_spec.treesitter({
            a = { "@block.outer", "@conditional.outer", "@loop.outer" },
            i = { "@block.inner", "@conditional.inner", "@loop.inner" },
          }, {}),
          f = ai.gen_spec.treesitter({ a = "@function.outer", i = "@function.inner" }, {}),
          c = ai.gen_spec.treesitter({ a = "@class.outer", i = "@class.inner" }, {}),
          t = { "<([%p%w]-)%f[^<%w][^<>]->.-</%1>", "^<.->().*()</[^/]->$" },
        },
      }
    end,
  },
  {
    "danymat/neogen",
    keys = {
      {
        "<Leader>cc",
        function()
          require("neogen").generate({})
        end,
        desc = "Neogen Comment",
      },
    },
    config = function()
      require("neogen").setup({
        snippet_engine = "luasnip",
      })
    end,
  },
}
