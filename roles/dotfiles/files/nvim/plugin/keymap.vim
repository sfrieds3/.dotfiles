" grepping {{{
function! Grep(...)
    return system(join([&grepprg] + [expandcmd(join(a:000, ' '))], ' '))
endfunction

command! -nargs=+ -complete=file_in_path -bar Grep cgetexpr Grep(<f-args>)
command! -nargs=+ -complete=file_in_path -bar LGrep lgetexpr Grep(<f-args>)

cnoreabbrev <expr> grep (getcmdtype() ==# ':' && getcmdline() ==# 'grep') ? 'Grep' : 'grep'
cnoreabbrev <expr> lgrep (getcmdtype() ==# ':' && getcmdline() ==# 'grep') ? 'LGrep' : 'lgrep'

" FGrep <pattern> -> quickfix
command! -nargs=1 FGrep lgetexpr system(&grepprg . ' ' . <q-args> . ' ' . expand('%'))
nnoremap <Space> :FGrep<Space>

" Grep <pattern> -> quickfix
"command! -nargs=1 Grep cgetexpr system(&grepprg . ' ' . <q-args>) | copen
nnoremap gsg :Grep<Space>

" view all todo in quickfix window
nnoremap \vt :exec('lvimgrep /todo/j %')<cr>:exec('lopen')<CR>
nnoremap \vT :exec('Rg todo')<CR>

" gitgrep for word under cursor in current file and open in location list
nnoremap gr :execute('FGrep ' . expand('<cword>'))<CR>

" gitgrep for word under cursor in current directory open in quickfix
nnoremap gR :exec('Grep ' . expand('<cword>'))<CR>
" }}}

" better scrolling
nnoremap <expr> <C-b> max([winheight(0) - 2, 1]) . '<C-u>' . (line('.') < 1         + winheight(0) ? 'H' : 'L')
nnoremap <expr> <C-f> max([winheight(0) - 2, 1]) . '<C-d>' . (line('.') > line('$') - winheight(0) ? 'L' : 'H')

" redir
command! -nargs=1 -complete=command -bar -range Redir silent call redir#Redir(<q-args>, <range>, <line1>, <line2>)

" quickly edit recorded macros (https://github.com/mhinz/vim-galore#quickly-edit-your-macros)
nnoremap <leader>m  :<c-u><c-r><c-r>='let @'. v:register .' = '. string(getreg(v:register))<cr><c-f><left>

" show global variables
nnoremap _v :<C-u>let g: v:<CR>
" show local variables
nnoremap _V :<C-u>let b: t: w:<CR>

" echo current highlight
nnoremap _h :echo synIDattr(synID(line('.'), col('.'), 1), 'name')<CR>

" Switch CWD to the directory of the open buffer
" TODO make into cmd
" nnoremap _Cd :cd %:p:h<cr>:pwd<cr>

" open scratch buffers
nnoremap \` :<C-U>Scratch<CR>

" poor man's c_CTRL-G/c_CTRL-T.. use c-j/c-k to move thru search res as typing
cnoremap <expr> <C-g> getcmdtype() =~ '[\/?]' ? '<CR>/<C-r>/' : '<C-g>'
cnoremap <expr> <C-t> getcmdtype() =~ '[\/?]' ? '<CR>?<C-r>/' : '<C-t>'

" smarter c-n and c-p in Cmdline
cnoremap <expr> <c-n> wildmenumode() ? "\<c-n>" : "\<down>"
cnoremap <expr> <c-p> wildmenumode() ? "\<c-p>" : "\<up>"

" list and be ready to jump to cword
nnoremap <F4> [I:let n = input('> ')<Bar>exe 'normal ' . n . '[\t'<CR>

" ilist
nnoremap \i :Ilist!<Space>
nnoremap \I :Ilist! <C-r>=expand('<cword>')<CR><CR>

" ijump
nnoremap gsj :ijump! <C-r>=expand('<cword>')<CR><CR>

" quick jump to tag under curosr
nnoremap gst :tjump /<C-r>=expand('<cword>')<CR><CR>

" g search
nnoremap \gw :g//#<Left><Left>
nnoremap \gW :g/<C-r>=expand('<cword>')<CR>/#<CR>

" quick search and replace
" https://github.com/romainl/minivimrc/blob/master/vimrc
nnoremap \rp :'{,'}s/\<<C-r>=expand('<cword>')<CR>\>//gc<Left><Left><Left>
nnoremap \ra :%s/\<<C-r>=expand('<cword>')<CR>\>//gc<Left><Left><Left>

" replace next/previous occurrence (. to repeat)
" nnoremap \rn *``cgn
" nnoremap \rp #``cgN

" replace last search term
nnoremap <expr>  _R  ':%s/' . @/ . '//gc<Left><Left><Left>'

" :help include-search shortcuts
nnoremap gsp :<C-u>psearch <C-r><C-w><CR>
nnoremap gsi [<C-i>
nnoremap gsd [<C-d>

" quick make to location list
nnoremap <F5> :lmake %<CR>

" Do and insert results of math equations via python
" from https://github.com/alerque/que-vim/blob/master/.config/nvim/init.vim
command! -nargs=+ Calc :r! python3 -c 'from math import *; print (<args>)'

" show list of digraphs -- special symbols
nnoremap \vd :help digraphs<cr>:179<cr>zt

" stay where you are on *
nnoremap <silent> * :let lloc = winsaveview()<cr>*:call winrestview(lloc)<cr>

" last changed text as an object
onoremap \_ :<C-U>execute 'normal! `[v`]'<CR>
