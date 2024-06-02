local M = {}

function M.setup()
  local cmp = require("cmp")

  local t = function(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
  end

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
    mapping = cmp.mapping.preset.insert({
      ["<c-e>"] = cmp.mapping(
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
      ["<c-space>"] = cmp.mapping({
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
      ["<C-n>"] = cmp.mapping.select_next_item(),
      ["<C-p>"] = cmp.mapping.select_prev_item(),
      ["<C-b>"] = cmp.mapping.scroll_docs(-4),
      ["<C-f>"] = cmp.mapping.scroll_docs(4),
      ["<C-q>"] = cmp.mapping.abort(),
      ["<Tab>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.confirm({ select = true })
        else
          fallback()
        end
      end, { "i", "s" }),
      ["<C-l>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          return cmp.complete_common_string()
        end
        fallback()
      end, { "i", "c" }),
    }),
    sources = cmp.config.sources({
      { name = "nvim_lsp" },
      { name = "nvim_lsp_signature_help" },
      { name = "luasnip", max_item_count = 10 },
      { name = "lazydev" },
      { name = "path" },
    }, {
      { name = "buffer", max_item_count = 10 },
    }),
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
    formatting = {
      format = require("lspkind").cmp_format({
        mode = "symbol",
        maxwidth = 50,
        ellipsis_char = "...",
      }),
    },
    view = {
      entries = {
        follow_cursor = true,
      },
    },
    experimental = {
      ghost_text = {
        hl_group = "LspCodeLens",
      },
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
