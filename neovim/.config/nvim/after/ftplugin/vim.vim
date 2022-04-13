setlocal shiftwidth=4 softtabstop=4 expandtab foldmethod=marker foldenable

nnoremap <buffer> K :silent execute ':help ' . expand('<cword>')<CR>

let b:undo_ftplugin = "setlocal sw< sts< et< fen< fdm<"
