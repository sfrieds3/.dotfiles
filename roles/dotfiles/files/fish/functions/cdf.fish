function cdf -d "cd to directory containing file"
  cd $(fd | fzf-tmux -p --print0 | xargs -0 dirname)
end
