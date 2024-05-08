local is_executable = vim.fn.executable

return {
  {
    "Vigemus/iron.nvim",
    config = function()
      local iron = require("iron.core")
      iron.setup({
        config = {
          scratch_repl = true,
          repl_definition = {
            sh = {
              command = { "zsh" },
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
    "Vonr/align.nvim",
    branch = "v2",
    keys = {
      {
        "aa",
        function()
          require("align").align_to_char({
            length = 1,
          })
        end,
        mode = "x",
        { desc = "Align to 1 Character" },
      },

      {
        "ad",
        function()
          require("align").align_to_char({
            preview = true,
            length = 2,
          })
        end,
        mode = "x",
        { desc = "Align to 2 Characters with Previews" },
      },

      {
        "aw",
        function()
          require("align").align_to_string({
            preview = true,
            regex = false,
          })
        end,
        mode = "x",
        { desc = "Align to String with Previews" },
      },

      {
        "ar",
        function()
          require("align").align_to_string({
            preview = true,
            regex = true,
          })
        end,
        mode = "x",
        { desc = "Align to Vim Regex with Previews" },
      },

      {
        "gaw",
        function()
          local a = require("align")
          a.operator(a.align_to_string, {
            regex = false,
            preview = true,
          })
        end,
        { desc = "Align Paragraph to a String with Previews" },
      },

      {
        "gaa",
        function()
          local a = require("align")
          a.operator(a.align_to_char)
        end,
        { desc = "Align Paragraph to 1 Character" },
      },
    },
  },
  {
    "kevinhwang91/nvim-ufo",
    dependencies = {
      "kevinhwang91/promise-async",
    },
    event = "BufRead",

    --stylua: ignore
    keys = {
      { "zR", function() require("ufo").openAllFolds() end, desc = "UFO Open All Folds" },
      { "zM", function() require("ufo").closeAllFolds() end, desc = "UFO Close All Folds" },
      {
        "<leader>cp",
        function()
          local winid = require("ufo").peekFoldedLinesUnderCursor()
          if not winid then
            vim.lsp.buf.hover()
          end
        end,
        desc = "UFO Peek Folded Lines Under Cursor"
      },
    },

    config = function()
      local handler = function(virtText, lnum, endLnum, width, truncate)
        local newVirtText = {}
        local suffix = (" 󰁂 %d "):format(endLnum - lnum)
        local sufWidth = vim.fn.strdisplaywidth(suffix)
        local targetWidth = width - sufWidth
        local curWidth = 0
        for _, chunk in ipairs(virtText) do
          local chunkText = chunk[1]
          local chunkWidth = vim.fn.strdisplaywidth(chunkText)
          if targetWidth > curWidth + chunkWidth then
            table.insert(newVirtText, chunk)
          else
            chunkText = truncate(chunkText, targetWidth - curWidth)
            local hlGroup = chunk[2]
            table.insert(newVirtText, { chunkText, hlGroup })
            chunkWidth = vim.fn.strdisplaywidth(chunkText)
            -- str width returned from truncate() may less than 2nd argument, need padding
            if curWidth + chunkWidth < targetWidth then
              suffix = suffix .. (" "):rep(targetWidth - curWidth - chunkWidth)
            end
            break
          end
          curWidth = curWidth + chunkWidth
        end
        table.insert(newVirtText, { suffix, "MoreMsg" })
        return newVirtText
      end

      require("ufo").setup({
        provider_selector = function(bufnr, filetype, buftype)
          return { "treesitter", "indent" }
        end,
        fold_virt_text_handler = handler,
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
    opts = {
      snippet_engine = "luasnip",
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
