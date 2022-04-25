nnoremap j gj
nnoremap k gk
nnoremap ^ g^
nnoremap $ g$
nnoremap gj j
nnoremap gk k
nnoremap g^ ^
nnoremap g$ $

" allow c-j/c-k for cycling through insert mode completions
inoremap <C-j> <C-n>
inoremap <C-k> <C-p>

" easy switch to prev buffer
nnoremap <BS> <C-^>

" default Y mapping is just.. wrong
nnoremap Y y$

" format with black
xnoremap _k :Black -q -<CR>

" easy access to black hole register
nnoremap \d "_d
xnoremap \d "_d
xnoremap \p "_dP

" don't clobber unnamed register when pasting over text
xnoremap <silent> p p:if v:register == '"'<Bar>let @@=@0<Bar>endif<cr>

" change word under cursor and set as last search pattern
nnoremap <silent> c<Tab> :let @/=expand('<cword>')<cr>cgn

" insert current line into command line
if !has('patch-8.0.1787')
    cnoremap <C-r><C-l> <C-r>=getline('.')<CR>
endif

" easy switch to split by \<number>
let i = 1
while i <= 9
    execute 'nnoremap <Leader>' . i . ' :' . i . 'wincmd w<CR>'
    let i = i + 1
endwhile

" buffer/tab switching
nnoremap gb :bnext<CR>
nnoremap gB :bprevious<CR>
nnoremap ]b :bnext<CR>
nnoremap [b :bprevious<CR>
nnoremap ]t :tabnext<CR>
nnoremap [t :tabprevious<CR>

" arglist / quickfix / location list shortcuts
nnoremap ]a :next<CR>
nnoremap [a :previous<CR>
nnoremap [A :first<CR>
nnoremap ]A :last<CR>
nnoremap ]q :cnext<CR>
nnoremap [q :cprevious<CR>
nnoremap [Q :cfirst<CR>
nnoremap ]Q :clast<CR>
nnoremap \q :cclose<CR>
nnoremap ]l :lnext<CR>
nnoremap [l :lprevious<CR>
nnoremap [L :lfirst<CR>
nnoremap ]L :llast<CR>
nnoremap \l :lclose<CR>
nnoremap \<BS> :cclose<Bar>lclose<CR>
nnoremap <UP> :cprev<CR>
nnoremap <DOWN> :cnext<CR>
nnoremap <LEFT> :colder<CR>
nnoremap <RIGHT> :cnewer<CR>

" Leader,{ and Leader,} move to top and bottom of indent region
nmap \{ <Plug>(VerticalRegionUp)
nmap \} <Plug>(VerticalRegionDown)
omap \{ <Plug>(VerticalRegionUp)
omap \} <Plug>(VerticalRegionDown)
if exists(':xmap')
    xmap \{ <Plug>(VerticalRegionUp)
    xmap \} <Plug>(VerticalRegionDown)
endif

" adjust indent of last edit
nnoremap \< :<C-U>'[,']<<CR>
nnoremap \> :<C-U>'[,']><CR>

" buffers and ready to switch
" nnoremap \b :buffers<CR>:b<Space>
nnoremap \b :B<CR>

" find in path
nnoremap \fd :find **/*

" redraw screen
nnoremap \! :redraw!<CR>

" highlight interesting words
nnoremap _1 :call hiwords#HiInterestingWord(1)<cr>
nnoremap _2 :call hiwords#HiInterestingWord(2)<cr>
nnoremap _3 :call hiwords#HiInterestingWord(3)<cr>
nnoremap _4 :call hiwords#HiInterestingWord(4)<cr>
nnoremap _5 :call hiwords#HiInterestingWord(5)<cr>
nnoremap _6 :call hiwords#HiInterestingWord(6)<cr>

" trim trailing whitespace
command! StripTrailingWhitespace call whitespace#StripTrailingWhitespace()
command! -range=% StripTrailingWhitespaceVisual '<,'> call whitespace#StripTrailingWhitespaceVisual()
nnoremap \w :StripTrailingWhitespace<CR>
xnoremap \w :StripTrailingWhitespaceVisual<CR>

" toggle list
nnoremap _L :<C-U>setlocal list! list?<CR>
if exists(':xnoremap')
    xnoremap _L :<C-U>setlocal list! list?<CR>gv
endif

" line number management
command! ToggleLineNum call lnum#ToggleLineNum()
nnoremap _n :ToggleLineNum<cr>

" show declaration
" from https://gist.github.com/romainl/a11c6952f012f1dd32c26fad4fa82e43
nnoremap sd :call showdecl#ShowDeclaration(0)<CR>
nnoremap sD :call showdecl#ShowDeclaration(1)<CR>

" substitute operator
nmap <silent> \s  m':set operatorfunc=substitute#Substitute<CR>g@

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

" cdo/cfdo if not available
" from: https://www.reddit.com/r/vim/comments/iiatq6/is_there_a_good_way_to_do_vim_global_find_and/
if !exists(':cdo')
    command! -nargs=1 -complete=command Cdo try | sil cfirst |
                \ while 1 | exec <q-args> | sil cn | endwhile |
            \ catch /^Vim\%((\a\+)\)\=:E\%(553\|42\):/ |
            \ endtry

    command! -nargs=1 -complete=command Cfdo try | sil cfirst |
                \ while 1 | exec <q-args> | sil cnf | endwhile |
            \ catch /^Vim\%((\a\+)\)\=:E\%(553\|42\):/ |
            \ endtry
endif

" better scrolling
nnoremap <expr> <C-b> max([winheight(0) - 2, 1]) . '<C-u>' . (line('.') < 1         + winheight(0) ? 'H' : 'L')
nnoremap <expr> <C-f> max([winheight(0) - 2, 1]) . '<C-d>' . (line('.') > line('$') - winheight(0) ? 'L' : 'H')

" redir
command! -nargs=1 -complete=command -bar -range Redir silent call redir#Redir(<q-args>, <range>, <line1>, <line2>)

" quickly edit recorded macros (https://github.com/mhinz/vim-galore#quickly-edit-your-macros)
nnoremap <leader>m  :<c-u><c-r><c-r>='let @'. v:register .' = '. string(getreg(v:register))<cr><c-f><left>

" toggle paste mode and enter insert mode
nnoremap _p :set paste! paste?<CR>i
" just toggle paste..
nnoremap _P :set paste! paste?<CR>

" toggle spell checking
nnoremap _s :<C-u>setlocal spell! spell?<CR>

" echo filetype
nnoremap _t :<C-u>set filetype?<CR>

" reload filetype plugins
nnoremap _T :<C-u>doautocmd filetypedetect BufRead<CR>

" echo current file full path
nnoremap _f :echo expand('%:p')<cr>

" git and diff shortcuts
nnoremap _gg :echo system('git branch && git status')<CR>
nnoremap _gd :echo system('git diff ' . expand('%'))<CR>
nnoremap _gD :!clear && git diff %<CR>
nnoremap _gb :GitBranch<CR>
nnoremap _dh :Diff HEAD<CR>
nnoremap _dd :Diff<CR>
nnoremap _do :diffoff<CR>
nnoremap <expr> _<space> ":\<C-u>".(&diff ? 'diffoff' : 'diffthis') . "\<CR>"

" quick shell command
nnoremap _! :!<Space>

" show all registers
nnoremap \y :<C-u>registers<CR>
" show marks
nnoremap \k :<C-u>marks<CR>
" command history
nnoremap \H :<C-u>history :<CR>
nnoremap \h q:
" search history
nnoremap \/ q/

" toggle showing tab, end-of-line, and trailing whitespace
nnoremap _l :<C-u>setlocal list! list?<CR>
if exists(':xnoremap')
    xnoremap _l :<C-u>setlocal list! list?<CR>gv
endif

" normal maps
nnoremap _m :<C-u>map<CR>
" buffer-local normal maps
nnoremap _M :<C-u>map <buffer><CR>
" show global variables
nnoremap _v :<C-u>let g: v:<CR>
" show local variables
nnoremap _V :<C-u>let b: t: w:<CR>

" echo current highlight
nnoremap _h :echo synIDattr(synID(line('.'), col('.'), 1), 'name')<CR>

" toggle line and column markers
nnoremap <F3> :set cursorline! cursorcolumn!<CR>
nnoremap \c :set cursorline! cursorline?<cr>
nnoremap \C :set cursorcolumn! cursorcolumn?<cr>

" Switch CWD to the directory of the open buffer
nnoremap _Cd :cd %:p:h<cr>:pwd<cr>

" open scratch buffers
nnoremap \` :<C-U>Scratch<CR>

" search for non-ASCII characters
nnoremap \a /[^\x00-\x7F]<CR>

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
nnoremap \rn *``cgn
nnoremap \rp #``cgN

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

" upper case last word using ctrl+u
inoremap <C-u> <Esc>gUiwea

" Shift-Tab enters actual tab
inoremap <S-Tab> <C-v><Tab>

" stay where you are on *
nnoremap <silent> * :let lloc = winsaveview()<cr>*:call winrestview(lloc)<cr>

" Do a bunch of stuff on c-l
nnoremap <C-l> :nohlsearch<cr>:diffupdate<cr>:syntax sync fromstart<cr><c-l>

" last changed text as an object
onoremap \_ :<C-U>execute 'normal! `[v`]'<CR>

if has('terminal') || has('nvim')
    " easy terminal exit
    tnoremap <esc> <C-\><C-n>
endif

nnoremap \ev :vsplit $MYVIMRC<cr>
nnoremap \es :source $MYVIMRC<cr> :echo 'sourced ' . $MYVIMRC<cr>
