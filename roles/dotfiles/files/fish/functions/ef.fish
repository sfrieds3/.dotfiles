function ef -d "edit file in nvim"
  fzf-tmux -p | xargs nvim
end
