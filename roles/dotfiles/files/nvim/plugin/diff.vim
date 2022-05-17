if exists('g:loaded_diff')
    finish
endif
let g:loaded_diff = 1

" similar to :help diff-original-file
" original: https://gist.github.com/romainl/7198a63faffdadd741e4ae81ae6dd9e6
function! diff#Diff(spec)
    vertical new
    setlocal bufhidden=wipe buftype=nofile nobuflisted noswapfile
        let cmd = "++edit #"
    if len(a:spec)
        let cmd = "!git -C " . shellescape(fnamemodify(finddir('.git', '.;'), ':p:h:h')) . " show " . a:spec . ":#"
    endif
    execute "read " . cmd
    silent 0d_
    diffthis
    wincmd p
    diffthis
    wincmd r
    wincmd h
endfunction

command! -nargs=? Diff call diff#Diff(<q-args>)
