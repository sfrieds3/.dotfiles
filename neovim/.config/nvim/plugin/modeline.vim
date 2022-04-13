if exists('g:loaded_modeline')
    finish
endif
let g:loaded_modeline = 1

" from https://vim.fandom.com/wiki/Modeline_magic
function! modeline#AppendModeline()
    let l:modeline = printf(" vim: set ts=%d sw=%d %set:",
                \ &tabstop, &shiftwidth, &expandtab ? '' : 'no')
    let l:modeline = substitute(&commentstring, "%s", l:modeline, "")
    call append(line('0'), l:modeline)
endfunction

command! Modeline call modeline#AppendModeline()
