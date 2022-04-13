" open quickfix or location-list automatically when there is something to show
" source: https://gist.github.com/romainl/56f0c28ef953ffc157f36cc495947ab3
augroup AutoQuickfix
    autocmd!
    autocmd QuickFixCmdPost [^l]* cwindow
    autocmd QuickFixCmdPost l* lwindow
augroup END
