function gaf -d "git add file"
  git ls-files -m -o --exclude-standard | fzf --print0 -m | xargs -0 -t -o git add
end
