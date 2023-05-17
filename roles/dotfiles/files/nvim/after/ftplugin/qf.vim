nnoremap <buffer> j j
nnoremap <buffer> k k
nnoremap <buffer> gj gj
nnoremap <buffer> gk gk
nnoremap <buffer> H :colder<CR>
nnoremap <buffer> L :cnewer<CR>

" use entire screen width for qf window
wincmd J

" set default qf height to 1/3 of window
let &l:winheight = &lines / 3
