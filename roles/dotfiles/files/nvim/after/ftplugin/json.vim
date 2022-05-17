setlocal shiftwidth=4 softtabstop=4
setlocal formatprg=python3\ -m\ json.tool

" pretty format json {{{
if !exists("*DoPrettyJSON")
    function! DoPrettyJSON()
        execute "%!python3 -m json.tool"
    endfunction
endif
command! PrettyJSON call DoPrettyJSON()
" }}}}

let b:undo_ftplugin = "setlocal sw< sts< fp<"
