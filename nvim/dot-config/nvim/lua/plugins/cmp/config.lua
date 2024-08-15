local M = {}

function M.setup()
  local cmp = require("cmp")

  local luasnip = require("luasnip")
  vim.keymap.set({ "i", "s" }, "<M-s>", function()
    if luasnip.locally_jumpable() then
      luasnip.jump(1)
    end
  end, { silent = true })

  vim.keymap.set({ "i", "s" }, "<M-S-s>", function()
    if luasnip.locally_jumpable(-1) then
      luasnip.jump(-1)
    end
  end, { silent = true })

  cmp.setup({
    snippet = {
      expand = function(args)
        require("luasnip").lsp_expand(args.body)
      end,
    },
    mapping = {
      ["<C-e>"] = cmp.mapping(
        cmp.mapping.confirm({
          behavior = cmp.ConfirmBehavior.Insert,
          select = true,
        }),
        { "i", "c" }
      ),

      ["<M-e>"] = cmp.mapping(
        cmp.mapping.confirm({
          behavior = cmp.ConfirmBehavior.Replace,
          select = false,
        }),
        { "i", "c" }
      ),

      ["<C-space>"] = cmp.mapping({
        i = cmp.mapping.complete({
          reason = cmp.ContextReason.Manual,
          config = {
            sources = {
              { name = "nvim_lsp_signature_help" },
            },
          },
        }),
        c = function(
          _ --[[fallback]]
        )
          if cmp.visible() then
            if not cmp.confirm({ select = true }) then
              return
            end
          else
            cmp.complete()
          end
        end,
      }),

      ["<Tab>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.confirm({ behavior = cmp.ConfirmBehavior.Insert, select = true })
        else
          fallback()
        end
      end, { "i", "s" }),

      ["<C-l>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.complete_common_string()
        end
        fallback()
      end, { "i", "c" }),

      ["<Down>"] = cmp.mapping(cmp.mapping.select_next_item(), { "i", "s", "c" }),
      ["<Up>"] = cmp.mapping(cmp.mapping.select_prev_item(), { "i", "s", "c" }),
      ["<C-n>"] = cmp.mapping(
        cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
        { "i", "s", "c" }
      ),
      ["<C-p>"] = cmp.mapping(
        cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }),
        { "i", "s", "c" }
      ),
      ["<C-b>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i" }),
      ["<C-f>"] = cmp.mapping(cmp.mapping.scroll_docs(4), { "i" }),
      ["<C-q>"] = cmp.mapping(cmp.mapping.abort(), { "i", "s", "c" }),
    },
    sources = cmp.config.sources({
      { name = "nvim_lsp" },
      { name = "nvim_lsp_signature_help" },
      { name = "luasnip" },
      { name = "lazydev" },
      { name = "path" },
    }, {
      {
        name = "buffer",
        option = {
          get_bufnrs = vim.api.nvim_list_bufs,
        },
      },
    }),
    ---@diagnostic disable-next-line: missing-fields
    sorting = {
      comparators = {
        cmp.config.compare.offset,
        cmp.config.compare.exact,
        cmp.config.compare.score,

        function(entry1, entry2)
          local _, entry1_under = entry1.completion_item.label:find("^_+")
          local _, entry2_under = entry2.completion_item.label:find("^_+")
          entry1_under = entry1_under or 0
          entry2_under = entry2_under or 0
          if entry1_under > entry2_under then
            return false
          elseif entry1_under < entry2_under then
            return true
          end
        end,

        cmp.config.compare.kind,
        cmp.config.compare.sort_text,
        cmp.config.compare.length,
        cmp.config.compare.order,
      },
    },
    ---@diagnostic disable-next-line: missing-fields
    formatting = {
      format = require("lspkind").cmp_format({
        mode = "symbol_text",
        maxwidth = 50,
        ellipsis_char = "...",
        symbol_map = { Codeium = "", Cody = "" },
      }),
    },
    view = {
      ---@diagnostic disable-next-line: missing-fields
      entries = {
        follow_cursor = true,
      },
    },
    experimental = {
      ghost_text = {
        hl_group = "LspCodeLens",
      },
    },
    ---@diagnostic disable-next-line: missing-fields
    performance = {
      max_view_entries = 15,
    },
  })

  cmp.setup.cmdline({ "/", "?" }, {
    completion = { autocomplete = false },
    sources = cmp.config.sources({
      { name = "nvim_lsp_document_symbol" },
    }, {
      { name = "buffer" },
    }),
  })

  cmp.setup.cmdline(":", {
    completion = { autocomplete = false },
    sources = cmp.config.sources({
      { name = "path" },
    }, {
      { name = "cmdline" },
    }),
  })
end

return M
