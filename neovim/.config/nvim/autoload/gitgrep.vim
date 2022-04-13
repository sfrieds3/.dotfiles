if exists('g:loaded_gitgrep')
    finish
endif
let g:loaded_gitgrep = 1

function! gitgrep#GitGrep(...)
    let args = ''
    for i in a:000
        let args = args . ' ' . i
    endfor
    let s = "cgetexpr system('git grep -in ' . args . ' *')""
    let s = s . ' | copen'
    execute s
    redraw!
endfunction
