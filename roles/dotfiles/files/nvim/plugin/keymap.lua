-- general nvim keymaps
vim.keymap.set('n', 'j', 'gj')
vim.keymap.set('n', 'k', 'gk')
vim.keymap.set('n', '^', 'g^')
vim.keymap.set('n', '$', 'g$')
vim.keymap.set('n', 'gj', 'j')
vim.keymap.set('n', 'gk', 'k')
vim.keymap.set('n', 'g^', '^')
vim.keymap.set('n', 'g$', '$')

-- easy access to black hole register
vim.keymap.set('n', '<Leader>d', '"_d')
vim.keymap.set('x', '<Leader>d', '"_d')
vim.keymap.set('x', '<Leader>p', '"_dP')

-- don't clobber unnamed register when pasting over text
vim.keymap.set('x', 'p', "p:if v:register == '\"'<Bar>let @@=@0<Bar>endif<cr>")

-- change word under cursor and set as last search pattern
vim.keymap.set('n', 'c<Tab>', ":let @/=expand('<cword>')<cr>cgn")

-- insert current line into command line
vim.keymap.set('c', '<C-r><C-l>', "<C-r>=getline('.')<CR>")

local i = 1
while (i <= 9) do
  vim.cmd("execute 'nnoremap <Leader>" .. i .. " :" .. i .. "wincmd w<CR>'")
  i = i + 1
end

-- buffer/tab switching
vim.keymap.set('n', 'gb', '<Cmd>bnext<CR>')
vim.keymap.set('n', 'gB', '<Cmd>bprevious<CR>')
vim.keymap.set('n', ']b', '<Cmd>bnext<CR>')
vim.keymap.set('n', '[b', '<Cmd>bprevious<CR>')
vim.keymap.set('n', ']t', '<Cmd>tabnext<CR>')
vim.keymap.set('n', '[t', '<Cmd>tabprevious<CR>')

-- arglist / quickfix / location list shortcuts
vim.keymap.set('n', ']a', '<Cmd>next<CR>')
vim.keymap.set('n', '[a', '<Cmd>previous<CR>')
vim.keymap.set('n', '[A', '<Cmd>first<CR>')
vim.keymap.set('n', ']A', '<Cmd>last<CR>')
vim.keymap.set('n', ']q', '<Cmd>cnext<CR>')
vim.keymap.set('n', '[q', '<Cmd>cprevious<CR>')
vim.keymap.set('n', '[Q', '<Cmd>cfirst<CR>')
vim.keymap.set('n', ']Q', '<Cmd>clast<CR>')
vim.keymap.set('n', '<Leader>q', '<Cmd>cclose<CR>')
vim.keymap.set('n', ']l', '<Cmd>lnext<CR>')
vim.keymap.set('n', '[l', '<Cmd>lprevious<CR>')
vim.keymap.set('n', '[L', '<Cmd>lfirst<CR>')
vim.keymap.set('n', ']L', '<Cmd>llast<CR>')
vim.keymap.set('n', '<Leader>l', '<Cmd>lclose<CR>')
vim.keymap.set('n', '<Leader><BS>', '<Cmd>cclose<Bar>lclose<CR>')

-- adjust indent of last edit
vim.keymap.set('n', '<Leader><', "<Cmd>'[,']<<CR>")
vim.keymap.set('n', '<Leader>>', "<Cmd>'[,']><CR>")

-- highlight interesting words
vim.keymap.set('n', '_1', ':call hiwords#HiInterestingWord(1)<cr>')
vim.keymap.set('n', '_2', ':call hiwords#HiInterestingWord(2)<cr>')
vim.keymap.set('n', '_3', ':call hiwords#HiInterestingWord(3)<cr>')
vim.keymap.set('n', '_4', ':call hiwords#HiInterestingWord(4)<cr>')
vim.keymap.set('n', '_5', ':call hiwords#HiInterestingWord(5)<cr>')
vim.keymap.set('n', '_6', ':call hiwords#HiInterestingWord(6)<cr>')

-- Leader,{ and Leader,} move to top and bottom of indent region
vim.keymap.set({ 'n', 'o', 'x' }, '<Leader>{', '<Plug>(VerticalRegionUp)')
vim.keymap.set({ 'n', 'o', 'x' }, '<Leader>}', '<Plug>(VerticalRegionDown)')

-- trim trailing whitespace
vim.api.nvim_create_user_command('StripTrailingWhitespace', 'call whitespace#StripTrailingWhitespace()', {})
vim.cmd[[command! -range=% StripTrailingWhitespaceVisual '<,'> call whitespace#StripTrailingWhitespaceVisual()]]
vim.keymap.set('n', '<Leader>w', '<Cmd>StripTrailingWhitespace<CR>')
vim.keymap.set('x', '<Leader>w', '<Cmd>StripTrailingWhitespaceVisual<CR>')

-- toggle list
vim.keymap.set({ 'n', 'x' }, '_L', '<Cmd>setlocal list! list?<CR>')

-- line number management
vim.api.nvim_create_user_command('ToggleLineNum', 'call lnum#ToggleLineNum()', {})
vim.keymap.set('n', '_n', '<Cmd>ToggleLineNum<CR>')

-- substitute operator
vim.keymap.set('n', '<Leader>s', "m':set operatorfunc=substitute#Substitute<CR>g@")

-- toggle spell checking
vim.keymap.set('n', '_s', '<Cmd>setocal spell! spell?<CR>')

-- echo filetype
vim.keymap.set('n', '_t', '<Cmd>set filetype?<CR>')

-- reload filetype plugins
vim.keymap.set('n', '_T', '<Cmd>doautocmd filetypedetect BufRead<CR>')

-- echo current file full path
vim.keymap.set('n', '_f', "<Cmd>echo expand('%:p')<CR>")

-- clear hlsearch highlights
vim.keymap.set('n', '<C-l>', '<Cmd>nohlsearch<CR>')
