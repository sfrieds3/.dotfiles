" do not include ':' as part of word
setlocal iskeyword-=:

setlocal shiftwidth=4 softtabstop=4 expandtab

setlocal foldmethod=indent nofoldenable

function! perl#Format() abort
    let fp = exists("g:pl_formatprg") ? g:pl_formatprg : 'perltidy\ -st'
    let lst = v:lnum + v:count - 1
    silent execute v:lnum . ',' . lst . '!' . fp
endfunction
setlocal formatexpr=perl#Format()

if exists("g:pl_makeprg")
    " e.g. let g:pl_makeprg='perl\c -c'
    execute "setlocal makeprg=" . g:pl_makeprg
else
    setlocal makeprg='perl\ -c'
endif

setlocal errorformat=%f:%l:\ %m

" settings for vim-perl
"let g:perl_include_pod = 1
"let g:perl_no_scope_in_variables = 0
"let g:perl_no_extended_vars = 0

" undo changes
let b:undo_ftplugin = "setlocal sw< sts< fdm< fen< isk< mp< et< fex< efm<"
