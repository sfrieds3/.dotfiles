function __git_wt_create_sanitize_branch -a branch
    string replace -ra '/' '-' -- $branch | string replace -ra '[^A-Za-z0-9._-]+' '-'
end

function __git_wt_create_next_dir -a branch
    set -l git_root (git rev-parse --path-format=absolute --git-common-dir 2>/dev/null)
    or return 1
    set -l base (set -q GIT_WORKTREE_BASE; and echo $GIT_WORKTREE_BASE; or echo $git_root)
    mkdir -p -- "$base" 2>/dev/null; or true

    set -l safe (__git_wt_create_sanitize_branch "$branch")
    set -l dir (string trim -r -c '/' -- $base)/$safe
    set -l n 0
    while test -e "$dir"
        set n (math $n + 1)
        set dir (string trim -r -c '/' -- $base)/$safe-$n
    end
    echo $dir
end

function __git_wt_create_branch_exists_local -a branch
    git show-ref --verify --quiet "refs/heads/$branch"
end

function __git_wt_create_branch_exists_remote -a remote branch
    git show-ref --verify --quiet "refs/remotes/$remote/$branch"
end

function __git_wt_create_find_remote -a branch
    git for-each-ref --format='%(refname:short)' "refs/remotes/*/$branch" \
        | sed -E 's#^remotes/([^/]+)/.*$#\1#' | head -n1
end

function git_wt_create -d "Create a git worktree for a branch (interactive or specified)"
    git rev-parse --git-dir >/dev/null 2>&1
    or begin
        echo "Not a git repo." >&2
        return 1
    end

    set -l git_common_dir (git rev-parse --path-format=absolute --git-common-dir 2>/dev/null)
    or return 1
    set -l main_worktree (string replace -r '/.git$' '' -- $git_common_dir)

    argparse 'r/remote=' -- $argv
    or return 1

    set -l remote (set -q GIT_WORKTREE_REMOTE; and echo $GIT_WORKTREE_REMOTE; or echo origin)
    if set -q _flag_remote
        set remote $_flag_remote
    end

    set -l branch $argv[1]
    set -l was_explicit_arg false
    if test -n "$branch"
        set was_explicit_arg true
    end

    if test -z "$branch"
        set -l choices (
            begin
                git for-each-ref --format='%(refname:short)' refs/heads
                git for-each-ref --format='%(refname:short)' refs/remotes | sed -E 's#^remotes/[^/]+/##'
            end | string match -rv '^(HEAD|origin/HEAD)$' | sort -u
        )
        or return 1

        set -l fzf_result
        set -l fzf_exit 0
        set fzf_result (printf '%s\n' $choices | fzf \
            --print-query \
            --prompt="branch > " \
            --header="Select branch (Enter=select match, Ctrl-T=use typed value)" \
            --bind='ctrl-t:clear-selection+accept' \
            --preview='git log --oneline --decorate -20 -- {1} 2>/dev/null || true' \
            --preview-window="right:50%" --reverse --border --ansi)
        or set fzf_exit $status

        if test $fzf_exit -eq 130
            return 130
        end

        if test (count $fzf_result) -ge 2
            set branch $fzf_result[2]
        else
            set branch $fzf_result[1]
        end
    end

    if string match -q '*/*' -- "$branch"
        set -l potential_remote (string split -m1 '/' -- $branch)[1]
        if git remote | string match -q -- $potential_remote
            set branch (string replace -r '^[^/]+/' '' -- $branch)
        end
    end

    if test -z "$branch"
        return 130
    end

    set -l dir (__git_wt_create_next_dir "$branch")
    or return 1

    if test "$was_explicit_arg" = true
        git -C "$main_worktree" worktree add -b "$branch" "$dir" >&2
        or return 1
    else
        if __git_wt_create_branch_exists_local "$branch"
            git -C "$main_worktree" worktree add "$dir" "$branch" >&2
            or return 1
        else
            set -l src_remote $remote
            if not __git_wt_create_branch_exists_remote $remote "$branch"
                set -l found_remote (__git_wt_create_find_remote "$branch")
                if test -n "$found_remote"
                    set src_remote $found_remote
                end
            end

            if test -n "$src_remote"
                git -C "$main_worktree" worktree add -b "$branch" "$dir" "$src_remote/$branch" >&2
                or return 1
            else
                git -C "$main_worktree" worktree add -b "$branch" "$dir" >&2
                or return 1
            end
        end
    end

    echo $dir
end

function __git_wt_create_binding
    set -l newdir (git_wt_create)
    and test -d "$newdir"
    and cd $newdir
    commandline -f repaint
end

function gwc -w git_wt_create
    git_wt_create $argv
end
