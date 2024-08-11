if exists('g:loaded_preserve')
    finish
endif
let g:loaded_preserve = 1

function! preserve#Preserve(command)
    try
        let l:win_view = winsaveview()
        execute 'keeppatterns keepjumps ' . a:command
    finally
        call winrestview(l:win_view)
    endtry
endfunction
