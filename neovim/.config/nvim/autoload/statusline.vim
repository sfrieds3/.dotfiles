if exists('g:loaded_statusline')
    finish
endif
let g:loaded_statusline = 1

function! statusline#ToggleStatusline() abort
    if &laststatus == 1
        set laststatus=2
    else
        set laststatus=1
    endif
    set laststatus?
endfunction

function! statusline#StatusLineWinAndBuffNum() abort
    let wnum = tabpagewinnr(tabpagenr())
    let bnum = expand(bufnr('%'))
    return printf("[%s]\ ", wnum)
endfunction

function! statusline#StatusLineFileName() abort
    let fname_str = winwidth(0) > 160 ? expand('%:F') : expand('%:t')
    return '' != fname_str ? printf("%s ", fname_str) : '[No Name] '
endfunction

function! statusline#StatusLineFiletype() abort
    return (strlen(&filetype) ? printf("(%s)", &filetype) : '(no ft)')
endfunction

function! statusline#StatusLineFormat() abort
    return winwidth(0) > 160 ? printf("%s | %s", &ff, &fenc) : ''
endfunction

function! statusline#TrailingWhitespace() abort
    return len(filter(getline(1, '$'), 'v:val =~ "\\s$"')) > 0 ? "[TRAIL]" : ""
endfunction

function! statusline#StatusLineGitBranch() abort
    let b = fugitive#statusline()
    return winwidth(0) > 80 ? printf("%s", b) : ''
endfunction

