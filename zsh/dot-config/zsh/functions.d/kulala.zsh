#!/bin/zsh
# Open kulala dir in the project root

function open-kulala() {
    local project_root=$(git rev-parse --path-format=absolute --git-common-dir | sed "s|/\.git/worktrees/.*||")

    if [ -n "$project_root" ]; then
        local http_files=$(fd . -e http "$project_root/kulala")

        local selected_file=$(echo $http_files | fzf \
            --prompt="kulala > " \
            --header="Select a kulala http file..." \
            --preview="bat --color=always --style=numbers --paging=never {}" \
            --preview-window="right:40%" \
            --delimiter='/' \
            --with-nth=-3,-2,-1 \
            --border \
            --ansi)

        if [ $? -ne 0 ]; then
            return 0
        fi

        if [ -n "$selected_file" ]; then
            nvim "$selected_file"
        fi
    fi
}

zle -N open-kulala
bindkey '^g^a' open-kulala
