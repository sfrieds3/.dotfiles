let g:undotree_WindowLayout = 2
nnoremap _U :exec('UndotreeToggle <bar> UndotreeFocus')<CR>
nnoremap \u :exec('UndotreeFocus')<CR>

function! g:Undotree_CustomMap()
    nmap <buffer> K <plug>UndotreeNextState
    nmap <buffer> J <plug>UndotreePreviousState
    nmap <buffer> \u q
endfunction
