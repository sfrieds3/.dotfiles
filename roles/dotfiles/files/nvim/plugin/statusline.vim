" Setup the colors
function! s:setup_colors() abort
    highlight StatuslineSeparator guifg=#3a3a3a gui=none guibg=none
    highlight StatuslinePercentage guibg=#3a3a3a gui=none guifg=#dab997
    highlight StatuslineNormal guibg=#3a3a3a gui=none guifg=#e9e9e9
    highlight StatuslineVC guibg=#3a3a3a gui=none guifg=#878787
    highlight StatuslineLintWarn guibg=#3a3a3a gui=none guifg=#ffcf00
    highlight StatuslineLintChecking guibg=#3a3a3a gui=none guifg=#458588
    highlight StatuslineLintError guibg=#3a3a3a gui=none guifg=#d75f5f
    highlight StatuslineLintOk guibg=#3a3a3a gui=none guifg=#b8bb26
    highlight StatuslineLint guibg=#e9e9e9 guifg=#3a3a3a
    highlight StatuslineLineCol guibg=#3a3a3a gui=none guifg=#878787
    highlight StatuslineTS guibg=#3a3a3a gui=none guifg=#878787
    highlight StatuslineFiletype guibg=#3a3a3a gui=none guifg=#e9e9e9
    highlight StatuslineNormalAccent guibg=#d75f5f gui=bold guifg=#e9e9e9
    highlight StatuslineInsertAccent guifg=#e9e9e9 gui=bold guibg=#dab997
    highlight StatuslineReplaceAccent guifg=#e9e9e9 gui=bold guibg=#afaf00
    highlight StatuslineConfirmAccent guifg=#e9e9e9 gui=bold guibg=#83adad
    highlight StatuslineTerminalAccent guifg=#e9e9e9 gui=bold guibg=#6f6f6f
    highlight StatuslineMiscAccent guifg=#e9e9e9 gui=bold guibg=#f485dd
    highlight StatuslineFilenameModified guifg=#d75f5f gui=bold,italic guibg=#3a3a3a
    highlight StatuslineFilenameNoMod guifg=#e9e9e9 gui=bold guibg=#3a3a3a
    highlight StatuslineFiletypeModified guifg=#d75f5f gui=italic guibg=#3a3a3a
    highlight StatuslineFiletypeNoMod guifg=#878787 gui=italic guibg=#3a3a3a
endfunction

" augroup statusline_colors
"   au!
"   au ColorScheme * call s:setup_colors()
" augroup END
"
" call s:setup_colors()
"
" au VimEnter * ++once lua statusline = require('scwfri.statusline')
" au VimEnter * ++once lua vim.o.statusline = '%!v:lua.statusline.status()'
