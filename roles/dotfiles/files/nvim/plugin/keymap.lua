-- space as leader
vim.g.mapleader = " "

-- general nvim keymaps
vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")
vim.keymap.set("n", "^", "g^")
vim.keymap.set("n", "$", "g$")
vim.keymap.set("n", "gj", "j")
vim.keymap.set("n", "gk", "k")
vim.keymap.set("n", "g^", "^")
vim.keymap.set("n", "g$", "$")

-- easy access to black hole register
vim.keymap.set("n", "<Leader>d", '"_d')
vim.keymap.set("x", "<Leader>d", '"_d')
vim.keymap.set("x", "<Leader>p", '"_dP')

vim.keymap.set("t", "<Esc>", "<C-\\><C-n>", { desc = "Map esc in terminal" })

vim.keymap.set("n", "\\x", "<Cmd>write<CR><Cmd>source %<CR>", { desc = "Quick source of current file" })

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

-- easy window switching
local i = 1
while i <= 9 do
  vim.cmd("execute 'nnoremap <Leader>" .. i .. " :" .. i .. "wincmd w<CR>'")
  i = i + 1
end

-- buffer/tab switching
vim.keymap.set("n", "gb", "<Cmd>bnext<CR>")
vim.keymap.set("n", "gB", "<Cmd>bprevious<CR>")
vim.keymap.set("n", "]b", "<Cmd>bnext<CR>")
vim.keymap.set("n", "[b", "<Cmd>bprevious<CR>")
vim.keymap.set("n", "\\q", "<Cmd>bclose<CR>")
vim.keymap.set("n", "]t", "<Cmd>tabnext<CR>")
vim.keymap.set("n", "[t", "<Cmd>tabprevious<CR>")
vim.keymap.set("n", "_Q", "<Cmd>tabclose<CR>")

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

-- highlight interesting words
vim.keymap.set("n", "_1", ":call hiwords#HiInterestingWord(1)<cr>")
vim.keymap.set("n", "_2", ":call hiwords#HiInterestingWord(2)<cr>")
vim.keymap.set("n", "_3", ":call hiwords#HiInterestingWord(3)<cr>")
vim.keymap.set("n", "_4", ":call hiwords#HiInterestingWord(4)<cr>")
vim.keymap.set("n", "_5", ":call hiwords#HiInterestingWord(5)<cr>")
vim.keymap.set("n", "_6", ":call hiwords#HiInterestingWord(6)<cr>")

vim.keymap.set(
  { "n", "o", "x" },
  "\\{",
  "<Plug>(VerticalRegionUp)",
  { desc = "VerticalRegion: move to top of indent region" }
)
vim.keymap.set(
  { "n", "o", "x" },
  "\\}",
  "<Plug>(VerticalRegionDown)",
  { desc = "VerticalRegion: move to bottom of indent region" }
)

-- trim trailing whitespace
vim.api.nvim_create_user_command("StripTrailingWhitespace", "call whitespace#StripTrailingWhitespace()", {})
vim.cmd([[command! -range=% StripTrailingWhitespaceVisual '<,'> call whitespace#StripTrailingWhitespaceVisual()]])
vim.keymap.set("n", "\\w", "<Cmd>StripTrailingWhitespace<CR>")
vim.keymap.set("x", "\\w", "<Cmd>StripTrailingWhitespaceVisual<CR>")

vim.keymap.set({ "n", "x" }, "_L", "<Cmd>setlocal list! list?<CR>", { desc = "Toggle list" })

-- line number management
vim.api.nvim_create_user_command("ToggleLineNum", "call lnum#ToggleLineNum()", {})
vim.keymap.set("n", "_n", "<Cmd>ToggleLineNum<CR>", { desc = "Toggle line numbers" })

vim.keymap.set("n", "\\s", "m':set operatorfunc=substitute#Substitute<CR>g@", { desc = "Substitute operator" })

vim.keymap.set("n", "_s", "<Cmd>setocal spell! spell?<CR>", { desc = "Toggle spellcheck" })

vim.keymap.set("n", "_t", "<Cmd>set filetype?<CR>", { desc = "Echo filetype" })

vim.keymap.set("n", "_T", "<Cmd>doautocmd filetypedetect BufRead<CR>", { desc = "Reload filetype plugins" })

vim.keymap.set("n", "_f", "<Cmd>echo expand('%:p')<CR>", { desc = "Echo full file path" })

vim.keymap.set("n", "<C-l>", "<Cmd>nohlsearch<CR>", { desc = "Clear hlsearch highlights" })

vim.keymap.set("n", "<Leader>c", "<Cmd>set cursorline! cursorline?<cr>", { desc = "Toggle cursorline" })
vim.keymap.set("n", "<Leader>C", "<Cmd>set cursorcolumn! cursorcolumn?<cr>", { desc = "Toggle cursorcolumn" })

vim.keymap.set("n", "_a", [[/[^\x00-\x7F]<CR>]], { desc = "Search for non-ASCII characters" })

vim.keymap.set("i", "<C-u>", "<Esc>gUiwea", { desc = "Upcase last word in insert mode" })

vim.keymap.set("i", "<S-Tab>", "<C-v><Tab>", { desc = "Insert actual <Tab> character in insert mode" })
