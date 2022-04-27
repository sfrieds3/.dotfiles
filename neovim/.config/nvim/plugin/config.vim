augroup ListChar
	autocmd!
	autocmd InsertEnter * set nolist
	autocmd InsertLeave * set list
augroup END

augroup Neovim
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank()
augroup END
packadd cfilter
