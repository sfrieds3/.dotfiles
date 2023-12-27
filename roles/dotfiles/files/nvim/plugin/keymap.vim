" better scrolling
nnoremap <expr> <C-b> max([winheight(0) - 2, 1]) . '<C-u>' . (line('.') < 1         + winheight(0) ? 'H' : 'L')
nnoremap <expr> <C-f> max([winheight(0) - 2, 1]) . '<C-d>' . (line('.') > line('$') - winheight(0) ? 'L' : 'H')

" redir
command! -nargs=1 -complete=command -bar -range Redir silent call redir#Redir(<q-args>, <range>, <line1>, <line2>)

" quickly edit recorded macros (https://github.com/mhinz/vim-galore#quickly-edit-your-macros)
nnoremap <Localleader>M  :<c-u><c-r><c-r>='let @'. v:register .' = '. string(getreg(v:register))<cr><c-f><left>

" smarter c-n and c-p in Cmdline
cnoremap <expr> <c-n> wildmenumode() ? "\<c-n>" : "\<down>"
cnoremap <expr> <c-p> wildmenumode() ? "\<c-p>" : "\<up>"

" quick search and replace
" https://github.com/romainl/minivimrc/blob/master/vimrc
nnoremap \rp :'{,'}s/\<<C-r>=expand('<cword>')<CR>\>//gc<Left><Left><Left>
nnoremap \ra :%s/\<<C-r>=expand('<cword>')<CR>\>//gc<Left><Left><Left>

" replace next/previous occurrence (. to repeat)
nnoremap \rn *``cgn
nnoremap \rp #``cgN

" replace last search term
nnoremap <expr>  _R  ':%s/' . @/ . '//gc<Left><Left><Left>'

" last changed text as an object
onoremap \_ :<C-U>execute 'normal! `[v`]'<CR>
