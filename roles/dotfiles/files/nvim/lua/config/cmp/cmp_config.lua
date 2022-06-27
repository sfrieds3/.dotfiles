local cmp = require('cmp')

local t = function(str)
  return vim.api.nvim_replace_termcodes(str, true, true, true)
end

cmp.setup {
  snippet = {
    expand = function(args)
      require('luasnip').lsp_expand(args.body)
    end,
  },
  mapping = {
    ['<Tab>'] = cmp.mapping.confirm({ select = true }),
    ['<C-n>'] = cmp.mapping.select_next_item(),
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<Down>'] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
    ['<Up>'] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
    ['<C-b>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
  },
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'nvim_lsp_signature_help' },
    { name = 'luasnip', max_item_count = 10 },
    { name = 'nvim_lua' },
    -- { name = 'path', max_item_count = 5 },
    -- { name = 'ripgrep' },
  }, {
    { name = 'buffer', max_item_count = 10 },
  }, {

    { name = 'tags', max_item_count = 10 },
  }),

  experimental = {
    ghost_text = true,
  },

  view = {
    -- entries = 'native',
  },
}

cmp.setup.cmdline('/', {
  completion = { autocomplete = false },
  sources = {
    { name = 'buffer',
      --opts = { keyword_pattern = [=[[^[:blank:]].*]=] }
    }
  }
})

cmp.setup.cmdline(':', {
  completion = { autocomplete = false },
  sources = cmp.config.sources({
    { name = 'path' }
  }, {
    { name = 'cmdline' }
  })
})
