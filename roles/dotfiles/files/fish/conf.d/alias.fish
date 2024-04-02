if status is-interactive
    # git aliases
    alias gs='git status'
    alias gba='git branch -a | fzf-tmux -p | sed s/^\*// | xargs git switch --detach'
    alias gb='git branch | fzf-tmux -p | sed s/^\*// | xargs git switch'
    alias gap='git add --patch'
    alias gcp='git checkout --patch'
    alias gdo='git diff origin/$(git rev-parse --abbrev-ref HEAD)'
    alias gdh='git diff origin/HEAD'
    alias gbp='git checkout -'
    alias gd='git diff'
    alias gdc='git diff --cached'
    alias gwip='git add -A; git rm $(git ls-files --deleted) 2> /dev/null; git commit --no-verify -m "[WIP]: $(date)"'
    alias gswip='git stash push -m "[WIP: $(git rev-parse --abbrev-ref HEAD)]: $(date)"'

    # other
    alias pcr="pre-commit run"
end
