if exists('g:loaded_whitespace')
    finish
endif
let g:loaded_whitespace = 1

function! whitespace#StripTrailingWhitespace()
    if !&binary && &filetype != 'diff'
        execute "redir => numclean"
        silent! execute "%s/\\s\\+$//en"
        execute "redir END"
        silent! call preserve#Preserve(":%s/\\s\\+$//e")
        if strlen(numclean) >0
            execute 'echo substitute(numclean, "\n", "", "g")'
        else
            execute 'echo "No trailing whitespace..."'
        endif
    endif
endfunction


function! whitespace#StripTrailingWhitespaceVisual() range
    if !&binary && &filetype != 'diff'
        let s:args = ":" . a:firstline . "," . a:lastline . "s/\\s\\+$//e"
        execute "redir => numclean"
        silent! execute s:args . "n"
        execute "redir END"
        silent! call preserve#Preserve(s:args)
        if strlen(numclean) >0
            execute 'echo substitute(numclean, "\n", "", "g")'
        else
            execute 'echo "No trailing whitespace..."'
        endif
    endif
endfunction
