function gs -d "Git status"
  git status
end

function gba -d "Swich to git branch (all)"
  git branch -a | fzf-tmux -p | sed s/^\*// | xargs git switch
end

function gb -d "Swich to git branch"
  git branch | fzf-tmux -p | sed s/^\*// | xargs git switch
end

function gap -d "Git add patch"
  git add --patch
end

function gcp -d "Git checkout patch"
  git checkout --patch
end

function gdo -d "Git diff origin"
  git diff origin/$(git rev-parse --abbrev-ref HEAD)
end

function gdh -d "Git diff origin/head"
  git diff origin/HEAD
end

function gbp -d "Git checkout previous branch"
  git checkout -
end

function gd -d "Git diff "
  git diff
end

function gdc -d "Git diff --cached"
  git diff --cached
end
