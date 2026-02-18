function cw -d "Quick switch to git worktree"
    set -l dir (git worktree list | fzf --prompt="Worktree: " --height 40% --reverse | awk '{print $1}')
    and cd $dir
end
