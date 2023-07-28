function glf -d "Git log full file"
  git ls-files --exclude-standard | fzf | xargs git lf
end

function cdf -d "cd to directory containing file"
  cd $(fd | fzf-tmux -p --print0 | xargs -0 dirname)
end

function cdd -d "cd to directory"
  cd $(fd -t d | fzf-tmux -p)
end

function ef -d "edit file in nvim"
  fzf-tmux -p | xargs nvim
end

function gaf -d "git add file"
  git ls-files -m -o --exclude-standard | fzf --print0 -m | xargs -0 -t -o git add
end

function gapf -d "git add patch file"
  git ls-files -m -o --exclude-standard | fzf --print0 -m | xargs -0 -t -o git add --patch
end

function gcpf -d "git checkout patch file"
  git ls-files -m -o --exclude-standard | fzf --print0 -m | xargs -0 -t -o git checkout --patch
end

function gdf -d "git diff file"
  git ls-files -m -o --exclude-standard | fzf-tmux -p | xargs git diff
end
