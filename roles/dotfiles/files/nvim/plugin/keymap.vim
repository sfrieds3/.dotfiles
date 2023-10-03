" better scrolling
nnoremap <expr> <C-b> max([winheight(0) - 2, 1]) . '<C-u>' . (line('.') < 1         + winheight(0) ? 'H' : 'L')
nnoremap <expr> <C-f> max([winheight(0) - 2, 1]) . '<C-d>' . (line('.') > line('$') - winheight(0) ? 'L' : 'H')

" redir
command! -nargs=1 -complete=command -bar -range Redir silent call redir#Redir(<q-args>, <range>, <line1>, <line2>)

" quickly edit recorded macros (https://github.com/mhinz/vim-galore#quickly-edit-your-macros)
nnoremap <Localleader>M  :<c-u><c-r><c-r>='let @'. v:register .' = '. string(getreg(v:register))<cr><c-f><left>

" show global variables
nnoremap _v :<C-u>let g: v:<CR>
" show local variables
nnoremap _V :<C-u>let b: t: w:<CR>

" open scratch buffers
nnoremap \` :<C-U>Scratch<CR>

" smarter c-n and c-p in Cmdline
cnoremap <expr> <c-n> wildmenumode() ? "\<c-n>" : "\<down>"
cnoremap <expr> <c-p> wildmenumode() ? "\<c-p>" : "\<up>"

" list and be ready to jump to cword
nnoremap <F4> [I:let n = input('> ')<Bar>exe 'normal ' . n . '[\t'<CR>

" ilist
nnoremap _i :Ilist!<Space>
nnoremap _I :Ilist! <C-r>=expand('<cword>')<CR><CR>

" ijump
nnoremap gsj :ijump! <C-r>=expand('<cword>')<CR><CR>

" quick jump to tag under curosr
nnoremap gst :tjump /<C-r>=expand('<cword>')<CR><CR>

" quick search and replace
" https://github.com/romainl/minivimrc/blob/master/vimrc
nnoremap \rp :'{,'}s/\<<C-r>=expand('<cword>')<CR>\>//gc<Left><Left><Left>
nnoremap \ra :%s/\<<C-r>=expand('<cword>')<CR>\>//gc<Left><Left><Left>

" replace next/previous occurrence (. to repeat)
nnoremap \rn *``cgn
nnoremap \rp #``cgN

" replace last search term
nnoremap <expr>  _R  ':%s/' . @/ . '//gc<Left><Left><Left>'

" :help include-search shortcuts
nnoremap gsp :<C-u>psearch <C-r><C-w><CR>
nnoremap gsi [<C-i>
nnoremap gsd [<C-d>

" last changed text as an object
onoremap \_ :<C-U>execute 'normal! `[v`]'<CR>
