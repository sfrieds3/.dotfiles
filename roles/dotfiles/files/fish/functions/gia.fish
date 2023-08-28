function gia --description "Git intent-to-add"
  git ls-files -o --exclude-standard | fzf --print0 -m | xargs -0 -t -o git add --intent-to-add
end
