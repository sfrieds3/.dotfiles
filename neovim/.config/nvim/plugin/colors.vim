hi def InterestingWord1 guifg=#000000 ctermfg=16 guibg=#ffa724 ctermbg=214
hi def InterestingWord2 guifg=#000000 ctermfg=16 guibg=#aeee00 ctermbg=154
hi def InterestingWord3 guifg=#000000 ctermfg=16 guibg=#8cffba ctermbg=121
hi def InterestingWord4 guifg=#000000 ctermfg=16 guibg=#b88853 ctermbg=137
hi def InterestingWord5 guifg=#000000 ctermfg=16 guibg=#ff9eb8 ctermbg=211
hi def InterestingWord6 guifg=#000000 ctermfg=16 guibg=#ff2c4b ctermbg=195

function! s:MyHighlights() abort
    highlight SignColumn        ctermbg=NONE cterm=NONE guibg=NONE gui=NONE

    if exists('g:colors_name') && g:colors_name ==# 'apprentice'
        highlight Comment ctermfg=242
        highlight Todo cterm=reverse ctermbg=226 ctermfg=235
    endif
    if exists('g:colors_name') && g:colors_name ==# 'lucius'
        LuciusDark
    endif
    if exists('g:colors_name') && g:colors_name ==# 'gooddog'
        "set cursorline
    endif
    if exists('g:colors_name') && g:colors_name ==# 'seoul256'
        highlight TabLine cterm=NONE gui=NONE
    endif
    if exists('g:colors_name') && g:colors_name ==# 'sierra'
        highlight link NonText Comment
        highlight link Whitespace Comment
        " No reverse on diffs
        highlight link DiffDelete Dark_Red
        highlight link diffChanged Dark_Red
        highlight link diffFile Dark_Red
        highlight link diffIndexLine Dark_Red
        highlight link diffRemoved Dark_Red
        highlight link DiffAdd Medium_Cyan
        highlight link DiffText Medium_Cyan
        highlight link diffAdded Medium_Cyan
        highlight link DiffChange Dark_Cyan
    endif

    let g:jellybeans_overrides = {
                \    'Todo': { 'guifg': '303030', 'guibg': 'f0f000',
                \              'ctermfg': 'Black', 'ctermbg': 'Yellow',
                \              'attr': 'bold' },
                \   'StatusLine': { 'gui': 'NONE' }
                \}
endfunction


augroup CustomizeTheme
    autocmd!
    autocmd ColorScheme * call s:MyHighlights()
augroup END

let g:sonokai_style = 'espresso'
colorscheme gruvbox-material
