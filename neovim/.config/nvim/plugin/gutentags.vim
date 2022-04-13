"let g:gutentags_generate_on_new = 0
"let g:gutentags_generate_on_missing = 0
let g:gutentags_cache_dir = '~/.vim/tmp/gutentags/'
if !isdirectory(expand(g:gutentags_cache_dir))
	call mkdir(expand(g:gutentags_cache_dir), "p")
endif
