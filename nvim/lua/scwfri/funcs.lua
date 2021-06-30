vim.cmd [[
" Run the last command
nnoremap <leader><leader>c :<up>

" Map execute this line
function! s:executor() abort
    if &ft == 'lua'
        call execute(printf(":lua %s", getline(".")))
    elseif &ft == 'vim'
        exe getline(".")
    endif
endfunction
nnoremap <leader>x :call <SID>executor()<CR>

" Execute file
function! s:save_and_exec() abort
    if &filetype == 'vim'
        :silent! write
        :source %
    elseif &filetype == 'lua'
        :silent! write
        :luafile %
    endif
    return
endfunction

nnoremap <leader><leader>x :call <SID>save_and_exec()<CR>

" Goes to the first line above/below that isn't whitespace
" http://vi.stackexchange.com/a/213
nnoremap <silent> gj :let _=&lazyredraw<CR>:set lazyredraw<CR>/\%<C-R>=virtcol(".")<CR>v\S<CR>:nohl<CR>:let &lazyredraw=_<CR>
nnoremap <silent> gk :let _=&lazyredraw<CR>:set lazyredraw<CR>?\%<C-R>=virtcol(".")<CR>v\S<CR>:nohl<CR>:let &lazyredraw=_<CR>
]]
