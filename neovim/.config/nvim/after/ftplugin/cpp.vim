setlocal suffixesadd=.h,.hpp

setlocal shiftwidth=4 softtabstop=4
setlocal foldmethod=syntax
setlocal nofoldenable

nnoremap <buffer> \<Space><Space> :FSHere<CR>
nnoremap <buffer> \<Space>h :FSSplitLeft<CR>
nnoremap <buffer> \<Space>j :FSSplitBelow<CR>
nnoremap <buffer> \<Space>k :FSSplitAbove<CR>
nnoremap <buffer> \<Space>l :FSSplitRight<CR>

function! cpp#Format() abort
    let fp = exists("g:cpp_formatprg") ? g:cpp_formatprg : 'clang-format'
    let lst = v:lnum + v:count - 1
    silent execute v:lnum . ',' . lst . '!' . fp
endfunction
setlocal formatexpr=cpp#Format()

let b:undo_ftplugin = "setlocal sw< sts< fdm< fen< sua< fex<"
