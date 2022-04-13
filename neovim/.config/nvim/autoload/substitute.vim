"credit: https://gist.github.com/romainl/b00ccf58d40f522186528012fd8cd13d
" Usage:
"   <key>ipfoo<CR>         Substitute every occurrence of the word under
"                          the cursor with 'foo' n the current paragraph
"   <key>Gfoo<CR>          Same, from here to the end of the buffer
"   <key>?bar<CR>foo<CR>   Same, from previous occurrence of 'bar'
"                          to current line
"

if exists('g:loaded_substitute')
    finish
endif
let g:loaded_substitute = 1

function! substitute#Substitute(type, ...)
    let cur = getpos("''")
    call cursor(cur[1], cur[2])
    let cword = expand('<cword>')
    execute "'[,']s/" . cword . "/" . input(cword . '/')
    call cursor(cur[1], cur[2])
endfunction
