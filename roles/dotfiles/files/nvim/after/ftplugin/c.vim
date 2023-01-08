if executable('clang')
    setlocal makeprg=clang
else
    setlocal makeprg=gcc
endif

setlocal suffixesadd=.h

"let &l:errorformat="%f:%l:%c:\ %t%s:\ %m"

setlocal shiftwidth=4 softtabstop=4
setlocal nofoldenable

let b:undo_ftplugin = "setlocal sw< sts< fen sua< mp"
