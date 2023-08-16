function cdd -d "cd to directory"
  cd $(fd -t d | fzf-tmux -p)
end
