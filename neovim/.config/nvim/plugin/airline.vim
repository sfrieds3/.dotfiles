"let g:airline#extensions#term#enabled = 1
"let g:airline#extensions#tabline#enabled = 1
"let g:airline#extensions#tabline#formatter = 'unique_tail_improved'
"let g:airline#extensions#tabline#show_buffers = 1
"let g:airline#extensions#searchcount#enabled = 0
"let g:airline#extensions#hunks#enabled = 0
"let g:airline#extensions#branch#format = 2

"call airline#parts#define_minwidth('branch', 120)
"call airline#parts#define_minwidth('tagbar', 120)
"call airline#parts#define_minwidth('searchcount', 120)
"call airline#parts#define_minwidth('whitespace', 120)
"call airline#parts#define_minwidth('ffenc', 120)

"let g:airline_symbols_ascii = 1
"if !exists('g:airline_symbols')
"    let g:airline_symbols = {}
"endif
""let g:airline_left_sep = ''
""let g:airline_left_alt_sep = ''
""let g:airline_right_sep = ''
""let g:airline_right_alt_sep = ''
"" let g:airline_symbols.branch = ''
"" let g:airline_symbols.colnr = ' :'
"" let g:airline_symbols.readonly = ''
"" let g:airline_symbols.linenr = ' :'
"" let g:airline_symbols.maxlinenr = '☰ '
"" let g:airline_symbols.paste = 'ρ'
"" let g:airline_symbols.whitespace = 'Ξ'

"function! SetAirlineExtensions()
"    if line('$') > 20000
"        call airline#extensions#whitespace#disable()
"    endif
"endfunction

"augroup AirlineExtensions
"    autocmd! BufNewFile,BufRead call SetAirlineExtensions()
"augroup END

"let g:airline_mode_map = {
"            \ '__'     : '-',
"            \ 'c'      : 'C',
"            \ 'i'      : 'I',
"            \ 'ic'     : 'I',
"            \ 'ix'     : 'I',
"            \ 'n'      : 'N',
"            \ 'multi'  : 'M',
"            \ 'ni'     : 'N',
"            \ 'no'     : 'N',
"            \ 'R'      : 'R',
"            \ 'Rv'     : 'R',
"            \ 's'      : 'S',
"            \ 'S'      : 'S',
"            \ '^S'     : 'S',
"            \ 't'      : 'T',
"            \ 'v'      : 'V',
"            \ 'V'      : 'V',
"            \ '^V'     : 'V',
"            \ }
