function fzf_worktree -d "fzf-based git worktree selector"
    set -l selected (git worktree list | fzf \
        --prompt="worktrees > " \
        --header="Select a worktree to cd into" \
        --preview="echo '📦 Branch:' && git -C {1} branch --show-current && echo '' && echo '📝 Changed files:' && git -C {1} status --porcelain | head -10 && echo '' && echo '📚 Recent commits:' && git -C {1} log --oneline --decorate -10" \
        --preview-window="right:40%" \
        --reverse --border --ansi)
    or return 0

    set -l selected_path (string split ' ' -- $selected)[1]
    if test -d "$selected_path"
        cd $selected_path
        commandline -f repaint
    else
        echo "Directory not found: $selected_path"
        return 1
    end
end
