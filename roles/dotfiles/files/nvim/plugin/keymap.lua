-- general nvim keymaps
vim.keymap.set({ "n", "x" }, "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
vim.keymap.set({ "n", "x" }, "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })

vim.keymap.set("n", "^", "g^")
vim.keymap.set("n", "$", "g$")
vim.keymap.set("n", "gj", "j")
vim.keymap.set("n", "gk", "k")
vim.keymap.set("n", "g^", "^")
vim.keymap.set("n", "g$", "$")

-- drag lines
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv", { desc = "Move line down one" })
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv", { desc = "Move line up one" })

-- better scrolling
vim.keymap.set(
  "n",
  "<C-b>",
  "max([winheight(0) - 2, 1]) . '<C-u>' . (line('.') < 1         + winheight(0) ? 'H' : 'L')",
  { desc = "Better scrolling with C-b", expr = true }
)
vim.keymap.set(
  "n",
  "<C-f>",
  "max([winheight(0) - 2, 1]) . '<C-d>' . (line('.') > line('$') - winheight(0) ? 'L' : 'H')",
  { desc = "Better scrolling with C-f", expr = true }
)

-- better c-n/c-p in cmdline -- match behavior of up/down
vim.keymap.set("c", "<C-n>", function()
  return vim.fn.wildmenumode() == 1 and "<C-n>" or "<down>"
end, { expr = true })
vim.keymap.set("c", "<C-p>", function()
  return vim.fn.wildmenumode() == 1 and "<C-P>" or "<up>"
end, { expr = true })

-- do not jump to first match with * or #
vim.keymap.set("n", "*", [[:let @/ = '\v' . expand('<cword>')<bar>set hlsearch<cr>]], { remap = true })
vim.keymap.set("n", "#", "#``", { remap = true })

-- easy access to black hole register
vim.keymap.set("n", "<leader>d", '"_d')
vim.keymap.set("x", "<leader>d", '"_d')
vim.keymap.set("x", "<leader>p", '"_dP')

vim.keymap.set({ "n", "v" }, "<leader>y", '"+y')
vim.keymap.set("n", "<leader>Y", '"+Y')

vim.keymap.set("t", "<Esc>", "<C-\\><C-n>", { desc = "Map esc in terminal" })

vim.keymap.set("n", "\\x", "<cmd>write<cr><Cmd>source %<CR>", { desc = "Quick source of current file" })

-- run make with <F5>
vim.keymap.set("n", "<F5>", "<cmd>make<cr>", { desc = "Make" })

vim.keymap.set(
  "x",
  "p",
  "p:if v:register == '\"'<Bar>let @@=@0<Bar>endif<cr>",
  { desc = "Don't clobber unnamed register when pasting over text" }
)

vim.keymap.set(
  "n",
  "c<Tab>",
  ":let @/=expand('<cword>')<cr>cgn",
  { desc = "Change word under cursor and set as last search pattern" }
)

vim.keymap.set("c", "<C-r><C-l>", "<C-r>=getline('.')<cr>", { desc = "Insert current line into cmdline" })

-- buffer/tab switching
vim.keymap.set("n", "gb", "<cmd>bnext<cr>")
vim.keymap.set("n", "gB", "<cmd>bprevious<cr>")
vim.keymap.set("n", "]b", "<cmd>bnext<cr>")
vim.keymap.set("n", "[b", "<cmd>bprevious<cr>")
vim.keymap.set("n", "\\q", "<cmd>bclose<cr>")
vim.keymap.set("n", "]t", "<cmd>tabnext<cr>")
vim.keymap.set("n", "[t", "<cmd>tabprevious<cr>")
vim.keymap.set("n", "_Q", "<cmd>tabclose<cr>")

-- tab management
vim.keymap.set("n", "<leader>tc", "<cmd>tabnew<cr>", { desc = "Create Tab" })
vim.keymap.set("n", "<leader>tC", "<cmd>tabclose<cr>", { desc = "Close Tab" })
vim.keymap.set("n", "<leader>tn", "<cmd>tabnext<cr>", { desc = "Next Tab" })
vim.keymap.set("n", "<leader>tp", "<cmd>tabprevious<cr>", { desc = "Previous Tab" })

-- arglist / quickfix / location list shortcuts
vim.keymap.set("n", "]a", "<cmd>next<cr>")
vim.keymap.set("n", "[a", "<cmd>previous<cr>")
vim.keymap.set("n", "[A", "<cmd>first<cr>")
vim.keymap.set("n", "]A", "<cmd>last<cr>")
vim.keymap.set("n", "[[q", "<cmd>colder<cr>")
vim.keymap.set("n", "]]q", "<cmd>cnewer<cr>")
vim.keymap.set("n", "]q", "<cmd>cnext<cr>")
vim.keymap.set("n", "[q", "<cmd>cprevious<cr>")
vim.keymap.set("n", "[Q", "<cmd>cfirst<cr>")
vim.keymap.set("n", "]Q", "<cmd>clast<cr>")
vim.keymap.set("n", "[[l", "<cmd>lolder<cr>")
vim.keymap.set("n", "]]l", "<cmd>lnewer<cr>")
vim.keymap.set("n", "]l", "<cmd>lnext<cr>")
vim.keymap.set("n", "[l", "<cmd>lprevious<cr>")
vim.keymap.set("n", "[L", "<cmd>lfirst<cr>")
vim.keymap.set("n", "]L", "<cmd>llast<cr>")
vim.keymap.set("n", "\\<BS>", "<cmd>cclose<Bar>lclose<cr>")

vim.keymap.set("n", "<leader><", "<cmd>'[,']<<cr>", { desc = "Dedent last edit" })
vim.keymap.set("n", "<leader>>", "<cmd>'[,']><cr>", { desc = "Indent last edit" })

-- keep selection when shifting
vim.keymap.set("x", "<", "<gv")
vim.keymap.set("x", ">", ">gv")

vim.keymap.set({ "n", "x" }, "_L", "<cmd>setlocal list! list?<cr>", { desc = "Toggle list" })

-- other various keymaps
vim.keymap.set("n", "_s", "<cmd>setocal spell! spell?<cr>", { desc = "Toggle spellcheck" })
vim.keymap.set("n", "_t", "<cmd>set filetype?<cr>", { desc = "Echo filetype" })
vim.keymap.set("n", "_T", "<cmd>doautocmd filetypedetect BufRead<cr>", { desc = "Reload filetype plugins" })
vim.keymap.set("n", "_f", "<cmd>echo expand('%:p')<cr>", { desc = "Echo full file path" })
vim.keymap.set("n", "<c-l>", "<cmd>nohlsearch<cr>", { desc = "Clear hlsearch highlights" })
vim.keymap.set("n", "\\c", "<cmd>set cursorline! cursorline?<cr>", { desc = "Toggle cursorline" })
vim.keymap.set("n", "\\C", "<cmd>set cursorcolumn! cursorcolumn?<cr>", { desc = "Toggle cursorcolumn" })
vim.keymap.set("n", "_a", [[/[^\x00-\x7F]<cr>]], { desc = "Search for non-ASCII characters" })
vim.keymap.set("i", "<c-u>", "<esc>gUiwea", { desc = "Upcase last word in insert mode" })
vim.keymap.set("i", "<s-tab>", "<C-v><tab>", { desc = "Insert actual <Tab> character in insert mode" })

-- quickly edit recorded macros (https://github.com/mhinz/vim-galore#quickly-edit-your-macros)
vim.keymap.set(
  "n",
  "<localleader>M",
  ":<c-u><c-r><c-r>='let @'. v:register .' = '. string(getreg(v:register))<cr><c-f><left>",
  { desc = "Quickly edit recorded macros" }
)

vim.keymap.set(
  "v",
  "<localleader>rp",
  "<cmd>'{,'}s/<<C-r>=expand('<cword>')<cr>>//gc<Left><Left><Left>",
  { desc = "Quick replace current visual word" }
)
vim.keymap.set(
  "n",
  "<localleader>ra",
  "<cmd>%s/<<C-r>=expand('<cword>')<cr>>//gc<Left><Left><Left>",
  { desc = "Quick replace current word" }
)

vim.keymap.set("n", "<localleader>rn", "*``cgn", { desc = "Replace next occurrence" })
vim.keymap.set("n", "<localleader>rp", "#``cgN", { desc = "Replace previous occurrence" })

-- last changed text as an object
vim.keymap.set("o", "<localleader>_", "<cmd><C-U>execute 'normal! `[v`]'<cr>", { desc = "Last Changed Text Object" })
-- select last pasted text
vim.keymap.set("n", "gp", "`[v`]")

-- highlights under cursor
vim.keymap.set("n", "<leader>ui", vim.show_pos, { desc = "Inspect Pos" })

-- replace last search term
vim.keymap.set("n", "_R", "':%s/' . @/ . '/'", { desc = "Replace last search term", expr = true })
