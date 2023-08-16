function glf -d "Git log full file"
  git ls-files --exclude-standard | fzf | xargs git lf
end
