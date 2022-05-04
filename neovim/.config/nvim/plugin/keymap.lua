local map = require("utils").mapper()

map("n", "j", "gj")
map("n", "k", "gk")
map("n", "^", "g^")
map("n", "$", "g$")
map("n", "gj", "j")
map("n", "gk", "k")
map("n", "g^", "^")
map("n", "g$", "$")


-- easy access to black hole register
map("n", "<Leader>d", "\"_d")
map("x", "<Leader>d", "\"_d")
map("x", "<Leader>p", "\"_dP")

-- don't clobber unnamed register when pasting over text
map("x", "p", "p:if v:register == '\"'<Bar>let @@=@0<Bar>endif<cr>")

-- change word under cursor and set as last search pattern
map("n", "c<Tab>", ":let @/=expand('<cword>')<cr>cgn")

-- insert current line into command line
map("c", "<C-r><C-l>", "<C-r>=getline('.')<CR>")

local i = 1
while (i <= 9) do
  vim.cmd("execute 'nnoremap <Leader>" .. i .. " :" .. i .. "wincmd w<CR>'")
  i = i + 1
end
