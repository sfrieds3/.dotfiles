if exists('g:loaded_lnum')
    finish
endif
let g:loaded_lnum = 1

function! lnum#ToggleLineNum()
    if &number || &relativenumber
        set nonumber norelativenumber number?
    else
        set number relativenumber number?
    endif
endfunction

