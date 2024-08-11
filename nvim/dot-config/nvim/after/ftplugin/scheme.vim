let b:is_chicken=1

setlocal lispwords+=let-values,condition-case,with-input-from-string
setlocal lispwords+=with-output-to-string,handle-exceptions,call/cc,rec,receive
setlocal lispwords+=call-with-output-file

" Indent a toplevel sexp.
function! Scheme_indent_top_sexp()
	let pos = getpos('.')
	silent! exec "normal! 99[(=%"
	call setpos('.', pos)
endfunction

nmap <buffer> <silent> == :call Scheme_indent_top_sexp()<cr>

setlocal include=\^\(\\(use\\\|require-extension\\)\\s\\+
setlocal includeexpr=substitute(v:fname,'$','.scm','')
setlocal path+=/usr/local/lib/chicken/3
setlocal suffixesadd=.scm
