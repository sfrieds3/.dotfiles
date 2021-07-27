local ls = require('luasnip')

ls.config.set_config {
  history = true,
  updateevents = "TextChanged,TextChangedI",
}

local snippet = ls.s
local snippet_from_nodes = ls.sn
local c = ls.c -- choice node
local f = ls.f -- function node
local i = ls.i -- insert node
local t = ls.t -- text node
local d = ls.d -- dynamic node

local str = function(text)
  return t { text }
end

local newline = function(text)
  return t { "", text }
end

local shortcut = function(val)
  if type(val) == "string" then
    return { t { val }, i(0) }
  end

  if type(val) == "table" then
    for k, v in ipairs(val) do
      if type(v) == "string" then
        val[k] = t { v }
      end
    end
  end

  return val
end

local make = function(tbl)
  local result = {}
  for k, v in pairs(tbl) do
    table.insert(result, (snippet({ trig = k, desc = v.desc }, shortcut(v))))
  end

  return result
end

local same = function(index)
  return f(function(args)
    return args[1]
  end, { index })
end

snippets = {}

snippets.python = make {
  fn = { "def ", i(1), "(", i(2), "):", newline "    ", i(3) },
  importpp = { "import pprint", newline "pp = pprint.PrettyPrinter(indent=4)" },
  pprint = { "pp.pprint(", i(i), ")" },
  main = { "def main():", newline "    ", i(1), newline "", newline "if __name_ == '__main_':", newline "    main()" },
  pd_print_df_full = { "with pd.option_context('display.max_rows', None, 'display.max_columns', None):", newline "    print(", i(1), ")", newline "" },
  pd_print_df_rows = { i(1), ".apply(lambda x: print(x), axis=1)", newline "" },
}

snippets.ruby = make {
  fn = { "def ", i(1), newline "  ", i(2), newline "end" },
  rcontroller = { "class ", i(1), " < ApplicationController", newline "  ", i(3), newline "end" },
}

snippets.eruby = make {

}
 
snippets.perl = make {

}

snippets.lua = make {
  lf = { 
    desc = "table function" ,
    "local ", i(1), " = function(", i(2), ")", newline "  ", i(0), newline "end",
  },
  f = { "function(", i(1), ")", i(0), newline "end" },
}

ls.snippets = snippets

vim.cmd [[
  imap <silent><expr> <c-k> luasnip#expand_or_jumpable() ? '<Plug>luasnip-expand-or-jump' : '<c-k>'
  inoremap <silent> <c-j> <cmd>lua require('luasnip').jump(-1)<CR>
  imap <silent><expr> <C-E> luasnip#choice_active() ? '<Plug>luasnip-next-choice' : '<C-E>'
  snoremap <silent> <c-k> <cmd>lua require('luasnip').jump(1)<CR>
  snoremap <silent> <c-j> <cmd>lua require('luasnip').jump(-1)<CR>
]]
