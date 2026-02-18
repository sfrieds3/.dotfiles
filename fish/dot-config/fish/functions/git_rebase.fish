function git_rebase -d "Interactive rebase from merge-base with origin branch"
    if test (count $argv) -ne 1
        echo "Usage: git_rebase <branch>"
        return 1
    end

    set -l base_branch $argv[1]
    set -l merge_base (git merge-base HEAD "origin/$base_branch")

    if test -z "$merge_base"
        echo "Error: Could not determine merge base with origin/$base_branch"
        return 1
    end

    git rebase --interactive "$merge_base~"
end
