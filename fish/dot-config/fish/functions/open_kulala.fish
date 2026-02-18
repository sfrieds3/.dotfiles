function open_kulala -d "Open a kulala HTTP file in nvim with fzf"
    set -l project_root (git rev-parse --path-format=absolute --git-common-dir | sed "s|/\.git/worktrees/.*||")

    if test -z "$project_root"
        return 1
    end

    set -l http_files (fd . -e http "$project_root/kulala")

    if test -z "$http_files"
        return 1
    end

    set -l selected_file (printf '%s\n' $http_files | fzf \
        --prompt="kulala > " \
        --header="Select a kulala http file..." \
        --preview="bat --color=always --style=numbers --paging=never {}" \
        --preview-window="right:40%" \
        --delimiter='/' \
        --with-nth=-3,-2,-1 \
        --border --ansi)
    or return 0

    if test -n "$selected_file"
        nvim "$selected_file"
    end
end
