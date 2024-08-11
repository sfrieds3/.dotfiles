" Map execute this line
function! funcs#executor() abort
    if &ft == 'lua'
        call execute(printf(":lua %s", getline(".")))
    elseif &ft == 'vim'
        exe getline(".")
    endif
endfunction
nnoremap \x :call funcs#executor()<CR>

" Execute file
function! funcs#save_and_exec() abort
    if &filetype == 'vim'
        :silent! write
        :source %
    elseif &filetype == 'lua'
        :silent! write
        :luafile %
    endif
    return
endfunction

nnoremap \\x :call funcs#save_and_exec()<CR>
