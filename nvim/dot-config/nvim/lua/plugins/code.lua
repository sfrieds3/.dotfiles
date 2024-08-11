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
