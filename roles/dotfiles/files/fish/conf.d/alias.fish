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

    # fzf aliaseg
    alias glf='git ls-files --exclude-standard | fzf | xargs git lf'
    alias cdf='cd $(fd | fzf-tmux -p --print0 | xargs -0 dirname)'
    alias cdd='cd $(fd -t d | fzf-tmux -p)'
    alias ef='fzf-tmux -p | xargs nvim'
    alias gaf='git ls-files -m -o --exclude-standard | fzf --print0 -m | xargs -0 -t -o git add'
    alias gapf='git ls-files -m -o --exclude-standard | fzf --print0 -m | xargs -0 -t -o git add --patch'
    alias gcpf='git ls-files -m -o --exclude-standard | fzf --print0 -m | xargs -0 -t -o git checkout --patch'
    alias gdf='git ls-files -m -o --exclude-standard | fzf-tmux -p | xargs git diff'

    # python
    alias condata="conda activate data"
    alias workonvenv="source $HOME/.venv/venv/bin/activate.fish"

    # other
    alias pcr="pre-commit run"
end
