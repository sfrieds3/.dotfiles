vim.cmd[[set runtimepath^=~/.vim runtimepath+=~/.vim/after runtimepath+=~/.config/nvim/colors runtimepath+=~/.config/nvim/pack]]
vim.cmd[[let &packpath = &runtimepath]]
vim.cmd[[set shell=/usr/bin/zsh]]
vim.cmd[[source ~/.vim/vimrc]]

vim.cmd[[nnoremap \ev :vsplit $MYVIMRC<cr>]]
vim.cmd[[nnoremap \es :luafile $MYVIMRC<cr> :echo 'sourced ' . $MYVIMRC<cr>]]
