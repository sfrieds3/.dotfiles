#!/bin/zsh

function occ() {
    if (( ! $+commands[opencode] )); then
        echo "opencode not found in PATH"
        return 1
    fi

    if (( ! $+commands[fzf] )); then
        echo "fzf not found in PATH"
        return 1
    fi

    local -a sessions picker_opts
    local selection session_id

    sessions=("${(@f)$(opencode session list 2>/dev/null)}")
    sessions=("${sessions[@]:3}")
    sessions=("${sessions[@]:#}")

    if (( ${#sessions[@]} == 0 )); then
        echo "No opencode sessions found"
        return 1
    fi

    picker_opts+=(
        --layout=reverse
        --border=rounded
    )

    if (( $+commands[jq] )); then
        picker_opts+=(
            --preview="NO_COLOR=1 opencode export {1} 2>/dev/null | jq -r 'def text_parts: [.parts[]? | select(.type == \"text\") | .text] | join(\"\\n\"); def role_name: if . == \"user\" then \"YOU\" elif . == \"assistant\" then \"AI\" else ascii_upcase end; \"Session: \" + (.info.id // \"-\"), \"Title:   \" + (.info.title // \"-\"), \"\", \"Recent messages\", \"--------------\", (.messages[-10:][] | .info.role as \$role | (text_parts | gsub(\"\\r\"; \"\") | gsub(\"\\n{3,}\"; \"\\n\\n\")) as \$text | select(\$text != \"\") | \"[\" + (\$role | role_name) + \"]\", (\$text | split(\"\\n\")[] | \"  \" + .), \"\")' 2>/dev/null || printf 'Preview unavailable\\n'"
            --preview-window="right:65%:wrap"
            --preview-label="Conversation"
        )
    fi

    if typeset -f __fzf_popup > /dev/null; then
        selection=$(printf '%s\n' "${sessions[@]}" | __fzf_popup --prompt="opencode sessions > " --header="Select session to continue" "${picker_opts[@]}")
    else
        selection=$(printf '%s\n' "${sessions[@]}" | fzf --prompt="opencode sessions > " --header="Select session to continue" "${picker_opts[@]}")
    fi

    [[ -z "$selection" ]] && return 0

    session_id=${selection%%[[:space:]]*}
    opencode --session "$session_id" "$@"
}
