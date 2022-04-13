let g:tagbar_autofocus = 1
let g:tagbar_autoclose = 1
let g:show_linenumbers = 1
nnoremap \t :echo tagbar#currenttag('[%s]', '')<CR>
nnoremap \\t :exec("TagbarOpen('j')")<cr>
