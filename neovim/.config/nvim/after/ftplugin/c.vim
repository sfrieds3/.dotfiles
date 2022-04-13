if executable('clang')
    setlocal makeprg=clang
else
    setlocal makeprg=gcc
endif

setlocal suffixesadd=.h

"let &l:errorformat="%f:%l:%c:\ %t%s:\ %m"

nnoremap <buffer> \<Space><Space> :FSHere<CR>
nnoremap <buffer> \<Space>h :FSSplitLeft<CR>
nnoremap <buffer> \<Space>j :FSSplitBelow<CR>
nnoremap <buffer> \<Space>k :FSSplitAbove<CR>
nnoremap <buffer> \<Space>l :FSSplitRight<CR>

setlocal shiftwidth=4 softtabstop=4
setlocal foldmethod=syntax
setlocal nofoldenable

let b:undo_ftplugin = "setlocal sw< sts< fdm< fen sua< mp"
