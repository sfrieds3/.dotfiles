
if exists("g:sh_makeprg")
    " e.g. let g:sh_makeprg='shellcheck'
    execute "setlocal makeprg=" . g:sh_makeprg
else
    setlocal makeprg='shellcheck'
endif

setlocal errorformat=%f:%l:%c\ %m

let b:undo_ftplugin = "setlocal mp< efm<"
