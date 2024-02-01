-- general nvim keymaps
vim.keymap.set({ "n", "x" }, "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
vim.keymap.set({ "n", "x" }, "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })

vim.keymap.set("n", "^", "g^")
vim.keymap.set("n", "$", "g$")
vim.keymap.set("n", "gj", "j")
vim.keymap.set("n", "gk", "k")
vim.keymap.set("n", "g^", "^")
vim.keymap.set("n", "g$", "$")

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

-- resize splits
vim.keymap.set("n", "<A-up>", "<C-w>+")
vim.keymap.set("n", "<A-down>", "<C-w>-")
vim.keymap.set("n", "<A-left>", "<C-w><")
vim.keymap.set("n", "<A-right>", "<C-w>>")

-- easy access to black hole register
vim.keymap.set("n", "<Leader>d", '"_d')
vim.keymap.set("x", "<Leader>d", '"_d')
vim.keymap.set("x", "<Leader>p", '"_dP')

vim.keymap.set("t", "<Esc>", "<C-\\><C-n>", { desc = "Map esc in terminal" })

vim.keymap.set("n", "\\x", "<Cmd>write<CR><Cmd>source %<CR>", { desc = "Quick source of current file" })

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

vim.keymap.set("c", "<C-r><C-l>", "<C-r>=getline('.')<CR>", { desc = "Insert current line into cmdline" })

-- buffer/tab switching
vim.keymap.set("n", "gb", "<Cmd>bnext<CR>")
vim.keymap.set("n", "gB", "<Cmd>bprevious<CR>")
vim.keymap.set("n", "]b", "<Cmd>bnext<CR>")
vim.keymap.set("n", "[b", "<Cmd>bprevious<CR>")
vim.keymap.set("n", "\\q", "<Cmd>bclose<CR>")
vim.keymap.set("n", "]t", "<Cmd>tabnext<CR>")
vim.keymap.set("n", "[t", "<Cmd>tabprevious<CR>")
vim.keymap.set("n", "_Q", "<Cmd>tabclose<CR>")

-- tab management
vim.keymap.set("n", "<leader>tc", "<cmd>tabnew<cr>", { desc = "Create Tab" })
vim.keymap.set("n", "<leader>tC", "<cmd>tabclose<cr>", { desc = "Close Tab" })
vim.keymap.set("n", "<leader>tn", "<cmd>tabnext<cr>", { desc = "Next Tab" })
vim.keymap.set("n", "<leader>tp", "<cmd>tabprevious<cr>", { desc = "Previous Tab" })

-- use right/left to switch tabs
vim.keymap.set("n", "<right>", function()
  vim.api.nvim_feedkeys("gt", "n", true)
end, { desc = "Next Tab" })
vim.keymap.set("n", "<left>", function()
  vim.api.nvim_feedkeys("gT", "n", true)
end, { desc = "Previous Tab" })

-- and up/down to switch buffers
vim.keymap.set("n", "<up>", "<cmd>bprevious<cr>", { desc = "Previous Buffer" })
vim.keymap.set("n", "<down>", "<cmd>bnext<cr>", { desc = "Next Buffer" })

-- arglist / quickfix / location list shortcuts
vim.keymap.set("n", "]a", "<Cmd>next<CR>")
vim.keymap.set("n", "[a", "<Cmd>previous<CR>")
vim.keymap.set("n", "[A", "<Cmd>first<CR>")
vim.keymap.set("n", "]A", "<Cmd>last<CR>")
vim.keymap.set("n", "[[q", "<Cmd>colder<CR>")
vim.keymap.set("n", "]]q", "<Cmd>cnewer<CR>")
vim.keymap.set("n", "]q", "<Cmd>cnext<CR>")
vim.keymap.set("n", "[q", "<Cmd>cprevious<CR>")
vim.keymap.set("n", "[Q", "<Cmd>cfirst<CR>")
vim.keymap.set("n", "]Q", "<Cmd>clast<CR>")
vim.keymap.set("n", "[[l", "<Cmd>lolder<CR>")
vim.keymap.set("n", "]]l", "<Cmd>lnewer<CR>")
vim.keymap.set("n", "]l", "<Cmd>lnext<CR>")
vim.keymap.set("n", "[l", "<Cmd>lprevious<CR>")
vim.keymap.set("n", "[L", "<Cmd>lfirst<CR>")
vim.keymap.set("n", "]L", "<Cmd>llast<CR>")
vim.keymap.set("n", "\\<BS>", "<Cmd>cclose<Bar>lclose<CR>")

vim.keymap.set("n", "<Leader><", "<Cmd>'[,']<<CR>", { desc = "Dedent last edit" })
vim.keymap.set("n", "<Leader>>", "<Cmd>'[,']><CR>", { desc = "Indent last edit" })

vim.keymap.set({ "n", "x" }, "_L", "<Cmd>setlocal list! list?<CR>", { desc = "Toggle list" })

-- substiture command
vim.keymap.set("n", "\\s", "m':set operatorfunc=substitute#Substitute<CR>g@", { desc = "Substitute operator" })

-- other various keymaps
vim.keymap.set("n", "_s", "<Cmd>setocal spell! spell?<CR>", { desc = "Toggle spellcheck" })
vim.keymap.set("n", "_t", "<Cmd>set filetype?<CR>", { desc = "Echo filetype" })
vim.keymap.set("n", "_T", "<Cmd>doautocmd filetypedetect BufRead<CR>", { desc = "Reload filetype plugins" })
vim.keymap.set("n", "_f", "<Cmd>echo expand('%:p')<CR>", { desc = "Echo full file path" })
vim.keymap.set("n", "<C-l>", "<Cmd>nohlsearch<CR>", { desc = "Clear hlsearch highlights" })
vim.keymap.set("n", "\\c", "<Cmd>set cursorline! cursorline?<cr>", { desc = "Toggle cursorline" })
vim.keymap.set("n", "\\C", "<Cmd>set cursorcolumn! cursorcolumn?<cr>", { desc = "Toggle cursorcolumn" })
vim.keymap.set("n", "_a", [[/[^\x00-\x7F]<CR>]], { desc = "Search for non-ASCII characters" })
vim.keymap.set("i", "<C-u>", "<Esc>gUiwea", { desc = "Upcase last word in insert mode" })
vim.keymap.set("i", "<S-Tab>", "<C-v><Tab>", { desc = "Insert actual <Tab> character in insert mode" })

-- quickly edit recorded macros (https://github.com/mhinz/vim-galore#quickly-edit-your-macros)
vim.keymap.set(
  "n",
  "<localleader>M",
  "<cmd><c-u><c-r><c-r>='let @'. v:register .' = '. string(getreg(v:register))<cr><c-f><left>",
  { desc = "Quickly edit recorded macros" }
)

vim.keymap.set(
  "v",
  "<localleader>rp",
  "<cmd>'{,'}s/<<C-r>=expand('<cword>')<CR>>//gc<Left><Left><Left>",
  { desc = "Quick replace current visual word" }
)
vim.keymap.set(
  "n",
  "<localleader>ra",
  "<cmd>%s/<<C-r>=expand('<cword>')<CR>>//gc<Left><Left><Left>",
  { desc = "Quick replace current word" }
)

vim.keymap.set("n", "<localleader>rn", "*``cgn", { desc = "Replace next occurrence" })
vim.keymap.set("n", "<localleader>rp", "#``cgN", { desc = "Replace previous occurrence" })

-- last changed text as an object
vim.keymap.set("o", "<localleader>_", "<cmd><C-U>execute 'normal! `[v`]'<CR>", { desc = "Last Changed Text Object" })

-- highlights under cursor
vim.keymap.set("n", "<leader>ui", vim.show_pos, { desc = "Inspect Pos" })

-- replace last search term
vim.keymap.set("n", "_R", "':%s/' . @/ . '/'", { desc = "Replace last search term", expr = true })
