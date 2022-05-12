local map = require("utils").mapper()

vim.g.tmux_navigator_no_mappings = 1
map("n", "<M-h>", "<Cmd>TmuxNavigateLeft<CR>")
map("n", "<M-j>", "<Cmd>TmuxNavigateDown<CR>")
map("n", "<M-k>", "<Cmd>TmuxNavigateUp<CR>")
map("n", "<M-l>", "<Cmd>TmuxNavigateRight<CR>")

map("n", "j", "gj")
map("n", "k", "gk")
map("n", "^", "g^")
map("n", "$", "g$")
map("n", "gj", "j")
map("n", "gk", "k")
map("n", "g^", "^")
map("n", "g$", "$")

-- easy access to black hole register
map("n", "<Leader>d", '"_d')
map("x", "<Leader>d", '"_d')
map("x", "<Leader>p", '"_dP')

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

-- buffer/tab switching
map("n", "gb", "<Cmd>bnext<CR>")
map("n", "gB", "<Cmd>bprevious<CR>")
map("n", "]b", "<Cmd>bnext<CR>")
map("n", "[b", "<Cmd>bprevious<CR>")
map("n", "]t", "<Cmd>tabnext<CR>")
map("n", "[t", "<Cmd>tabprevious<CR>")

-- arglist / quickfix / location list shortcuts
map("n", "]a", "<Cmd>next<CR>")
map("n", "[a", "<Cmd>previous<CR>")
map("n", "[A", "<Cmd>first<CR>")
map("n", "]A", "<Cmd>last<CR>")
map("n", "]q", "<Cmd>cnext<CR>")
map("n", "[q", "<Cmd>cprevious<CR>")
map("n", "[Q", "<Cmd>cfirst<CR>")
map("n", "]Q", "<Cmd>clast<CR>")
map("n", "<Leader>q", "<Cmd>cclose<CR>")
map("n", "]l", "<Cmd>lnext<CR>")
map("n", "[l", "<Cmd>lprevious<CR>")
map("n", "[L", "<Cmd>lfirst<CR>")
map("n", "]L", "<Cmd>llast<CR>")
map("n", "<Leader>l", "<Cmd>lclose<CR>")
map("n", "<Leader><BS>", "<Cmd>cclose<Bar>lclose<CR>")

-- adjust indent of last edit
map("n", "<Leader><", "<Cmd>'[,']<<CR>")
map("n", "<Leader>>", "<Cmd>'[,']><CR>")


map("n", "_1", ":call hiwords#HiInterestingWord(1)<cr>")
map("n", "_2", ":call hiwords#HiInterestingWord(2)<cr>")
map("n", "_3", ":call hiwords#HiInterestingWord(3)<cr>")
map("n", "_4", ":call hiwords#HiInterestingWord(4)<cr>")
map("n", "_5", ":call hiwords#HiInterestingWord(5)<cr>")
map("n", "_6", ":call hiwords#HiInterestingWord(6)<cr>")
