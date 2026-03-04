#!/bin/zsh

function _occ_preview() {
    local sid="$1"
    [[ "$sid" =~ ^ses_[a-zA-Z0-9]+$ ]] || { printf 'Invalid session ID\n'; return 1; }

    if NO_COLOR=1 opencode export "$sid" 2>/dev/null \
        | jq -r '
            def text_parts: [.parts[]? | select(.type == "text") | .text] | join("\n");
            def role_name: if . == "user" then "YOU" elif . == "assistant" then "AI" else ascii_upcase end;
            "Session: " + (.info.id // "-"),
            "Title:   " + (.info.title // "-"),
            "",
            "Recent messages",
            "--------------",
            (.messages[-10:][] | .info.role as $role |
                (text_parts | gsub("\r"; "") | gsub("\n{3,}"; "\n\n")) as $text |
                select($text != "") |
                "[" + ($role | role_name) + "]",
                ($text | split("\n")[] | "  " + .),
                "")' 2>/dev/null; then
        return 0
    fi

    local title
    title=$(opencode db "SELECT COALESCE(NULLIF(title, ''), '-') AS t FROM session WHERE id = '$sid'" 2>/dev/null | tail -n +2)
    if [[ -n "$title" ]]; then
        printf 'Session: %s\nTitle:   %s\n\nRecent messages\n--------------\n' "$sid" "$title"
        opencode db "
            SELECT
                CASE json_extract(m.data, '\$.role')
                    WHEN 'user' THEN '[YOU]'
                    WHEN 'assistant' THEN '[AI]'
                    ELSE '[' || UPPER(json_extract(m.data, '\$.role')) || ']'
                END || char(10) || '  ' || REPLACE(SUBSTR(json_extract(p.data, '\$.text'), 1, 500), char(10), char(10) || '  ') || char(10) AS preview
            FROM part p
            JOIN message m ON p.message_id = m.id
            WHERE p.session_id = '$sid'
                AND json_extract(p.data, '\$.type') = 'text'
                AND LENGTH(json_extract(p.data, '\$.text')) > 0
            ORDER BY p.time_created DESC
            LIMIT 10
        " 2>/dev/null | tail -n +2
        return 0
    fi

    printf 'Preview unavailable\n'
    return 1
}

function occ() {
    if (( ! $+commands[opencode] )); then
        echo "opencode not found in PATH"
        return 1
    fi

    if (( ! $+commands[fzf] )); then
        echo "fzf not found in PATH"
        return 1
    fi

    local -a sessions picker_opts forward_args
    local selection session_id picker_header
    local max_count=200
    local scope="current"
    local scope_mode="current"
    local scope_dir=""
    local arg
    local i=1

    while (( i <= $# )); do
        arg="${@[i]}"
        case "$arg" in
            --all|-a)
                max_count=2000
                ;;
            --scope|-s)
                (( i++ ))
                if (( i > $# )); then
                    echo "Missing value for $arg"
                    return 1
                fi
                scope="${@[i]}"
                ;;
            --scope=*)
                scope="${arg#--scope=}"
                ;;
            *)
                forward_args+=("$arg")
                ;;
        esac
        (( i++ ))
    done

    if [[ "$scope" == "all" ]]; then
        scope_mode="all"
    elif [[ "$scope" == "current" || "$scope" == "cwd" || "$scope" == "." ]]; then
        scope_mode="current"
    else
        scope_mode="directory"
        scope_dir="$scope"
        if [[ ! -d "$scope_dir" ]]; then
            echo "Scope directory not found: $scope_dir"
            return 1
        fi
    fi

    if [[ "$scope_mode" == "all" ]]; then
        sessions=("${(@f)$(opencode db "SELECT id || char(9) || COALESCE(NULLIF(title, ''), '-') FROM session ORDER BY time_updated DESC LIMIT $max_count" 2>/dev/null)}")
    elif (( $+commands[jq] )); then
        if [[ "$scope_mode" == "directory" ]]; then
            sessions=("${(@f)$( (builtin cd -- "$scope_dir" 2>/dev/null && opencode session list --format json --max-count "$max_count" 2>/dev/null | jq -r '.[] | [.id, (.title // "-")] | @tsv' 2>/dev/null) )}")
        else
            sessions=("${(@f)$(opencode session list --format json --max-count "$max_count" 2>/dev/null | jq -r '.[] | [.id, (.title // "-")] | @tsv' 2>/dev/null)}")
        fi
    else
        if [[ "$scope_mode" == "directory" ]]; then
            sessions=("${(@f)$( (builtin cd -- "$scope_dir" 2>/dev/null && opencode session list --max-count "$max_count" 2>/dev/null) )}")
        else
            sessions=("${(@f)$(opencode session list --max-count "$max_count" 2>/dev/null)}")
        fi
        sessions=("${(@M)sessions:#ses_*}")
    fi

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
            --preview="_occ_preview {1}"
            --preview-window="right:65%:wrap"
            --preview-label="Conversation"
        )
    fi

    picker_header="Select session to continue"
    if [[ "$scope_mode" == "all" ]]; then
        picker_header+=" (scope: all)"
    elif [[ "$scope_mode" == "directory" ]]; then
        picker_header+=" (scope: $scope_dir)"
    fi

    if typeset -f __fzf_popup > /dev/null; then
        selection=$(printf '%s\n' "${sessions[@]}" | __fzf_popup --prompt="opencode sessions > " --header="$picker_header" "${picker_opts[@]}")
    else
        selection=$(printf '%s\n' "${sessions[@]}" | fzf --prompt="opencode sessions > " --header="$picker_header" "${picker_opts[@]}")
    fi

    [[ -z "$selection" ]] && return 0

    session_id=${selection%%[[:space:]]*}
    opencode --session "$session_id" "${forward_args[@]}"
}
