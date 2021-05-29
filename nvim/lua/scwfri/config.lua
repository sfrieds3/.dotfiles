
vim.cmd[[nnoremap \ev :vsplit $MYVIMRC<cr>]]
vim.cmd[[nnoremap \es :luafile $MYVIMRC<cr> :echo 'sourced ' . $MYVIMRC<cr>]]
