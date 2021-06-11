vim.cmd([[ 
  set shell=/usr/bin/zsh
  source ~/.vim/vimrc

  nnoremap \ev :vsplit $MYVIMRC<cr>
  nnoremap \es :luafile $MYVIMRC<cr> :echo 'sourced ' . $MYVIMRC<cr>
]])
