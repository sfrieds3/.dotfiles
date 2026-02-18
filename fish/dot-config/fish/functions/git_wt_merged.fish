function git_wt_merged -d "Show worktree merge status relative to a target branch"
    git rev-parse --git-dir >/dev/null 2>&1
    or begin
        echo "Not a git repo." >&2
        return 1
    end

    set -l git_common_dir (git rev-parse --path-format=absolute --git-common-dir 2>/dev/null)
    or return 1

    set -l remote (set -q GIT_WORKTREE_REMOTE; and echo $GIT_WORKTREE_REMOTE; or echo origin)
    set -l target_branch $argv[1]

    if test -z "$target_branch"
        if git show-ref --verify --quiet "refs/remotes/$remote/develop"
            set target_branch "$remote/develop"
        else if git show-ref --verify --quiet "refs/remotes/$remote/main"
            set target_branch "$remote/main"
        else if git show-ref --verify --quiet "refs/remotes/$remote/master"
            set target_branch "$remote/master"
        else
            echo "No $remote/develop, $remote/main, or $remote/master found." >&2
            return 1
        end
    else if not string match -q '*/*' -- "$target_branch"
        if git show-ref --verify --quiet "refs/remotes/$remote/$target_branch"
            set target_branch "$remote/$target_branch"
        else if not git show-ref --verify --quiet "refs/heads/$target_branch"
            echo "Branch '$target_branch' not found locally or on $remote." >&2
            return 1
        end
    end

    set -l worktree_output (git worktree list --porcelain)
    set -l target_branch_short (string replace -r '^[^/]+/' '' -- $target_branch)

    set -l wt_path
    set -l wt_branch
    set -l merged
    set -l stale
    set -l unmerged

    for line in $worktree_output
        if string match -qr '^worktree ' -- "$line"
            set wt_path (string replace 'worktree ' '' -- $line)
        else if string match -qr '^branch ' -- "$line"
            set wt_branch (string replace 'branch refs/heads/' '' -- $line)
            if test "$wt_branch" != "$target_branch_short"
                set -l ahead (git -C "$git_common_dir" rev-list --count "$target_branch..$wt_branch" 2>/dev/null)
                set -l behind (git -C "$git_common_dir" rev-list --count "$wt_branch..$target_branch" 2>/dev/null)
                if test "$ahead" = 0
                    if test "$behind" = 0
                        set -a merged "$wt_branch|$wt_path"
                    else
                        set -a stale "$behind|$wt_branch|$wt_path"
                    end
                else
                    set -a unmerged "$ahead|$behind|$wt_branch|$wt_path"
                end
            end
            set wt_path
            set wt_branch
        end
    end

    echo "Worktrees compared to '$target_branch':"
    echo ""

    if test (count $merged) -gt 0
        echo "Merged (safe to remove):"
        for entry in $merged
            set -l parts (string split '|' -- $entry)
            echo "  ✓ $parts[1]"
            echo "    └─ $parts[2]"
        end
        echo ""
    end

    if test (count $stale) -gt 0
        echo "Stale (no changes, just behind):"
        for entry in $stale
            set -l parts (string split '|' -- $entry)
            echo "  ○ $parts[2] ($parts[1] behind)"
            echo "    └─ $parts[3]"
        end
        echo ""
    end

    if test (count $unmerged) -gt 0
        echo "Unmerged (has unique commits):"
        for entry in $unmerged
            set -l parts (string split '|' -- $entry)
            echo "  ● $parts[3] (+$parts[1], -$parts[2])"
            echo "    └─ $parts[4]"
        end
        echo ""
    end

    if test (count $merged) -eq 0 -a (count $stale) -eq 0 -a (count $unmerged) -eq 0
        echo "  (none)"
    end
end

function gwm -w git_wt_merged
    git_wt_merged $argv
end
