#!/bin/zsh

function _pic_preview() {
    local session_file="$1"
    [[ -f "$session_file" ]] || {
        printf 'Session file not found\n'
        return 1
    }

    if (( $+commands[jq] )); then
        jq -r '
            def text_parts:
                if (.message.content | type) == "string" then .message.content
                else [(.message.content // [])[]? | select(.type == "text") | .text] | join("\n")
                end;
            def role_name:
                if . == "user" then "YOU"
                elif . == "assistant" then "AI"
                else ascii_upcase
                end;
            first(.[] | select(.type == "session")) as $session |
            (
                [ .[] | select(.type == "session_info" and (.name // "") != "") | .name ] | last
            ) as $name |
            "Session: " + ($session.id // "-") + "\n" +
            "CWD:     " + ($session.cwd // "-") + "\n" +
            "Name:    " + ($name // "-") + "\n\n" +
            "Recent messages\n" +
            "--------------\n" +
            (
                [ .[]
                  | select(.type == "message")
                  | .message.role as $role
                  | (text_parts | gsub("\r"; "") | gsub("\n{3,}"; "\n\n")) as $text
                  | select($text != "")
                  | "[" + ($role | role_name) + "]\n" + ($text | split("\n") | map("  " + .) | join("\n"))
                ]
                | .[-10:]
                | join("\n\n")
            )
        ' "$session_file" 2>/dev/null
        return 0
    fi

    printf '%s\n' "$session_file"
}

function pic() {
    if (( ! $+commands[pi] )); then
        echo "pi not found in PATH"
        return 1
    fi

    if (( ! $+commands[fzf] )); then
        echo "fzf not found in PATH"
        return 1
    fi

    if (( ! $+commands[python3] )); then
        echo "python3 not found in PATH"
        return 1
    fi

    local agent_dir="${PI_CODING_AGENT_DIR:-$HOME/.pi/agent}"
    local sessions_dir="$agent_dir/sessions"
    local selection session_file
    local -a sessions picker_opts

    if [[ ! -d "$sessions_dir" ]]; then
        echo "Sessions directory not found: $sessions_dir"
        return 1
    fi

    sessions=("${(@f)$(python3 - "$sessions_dir" <<'PY'
import json
import sys
from pathlib import Path

root = Path(sys.argv[1])
rows = []

for path in root.rglob('*.jsonl'):
    try:
        cwd = '-'
        name = ''
        first_user = ''
        with path.open() as f:
            for i, line in enumerate(f):
                entry = json.loads(line)
                if i == 0 and entry.get('type') == 'session':
                    cwd = entry.get('cwd', '-')
                elif entry.get('type') == 'session_info' and entry.get('name'):
                    name = entry['name']
                elif not first_user and entry.get('type') == 'message':
                    message = entry.get('message', {})
                    if message.get('role') == 'user':
                        content = message.get('content', '')
                        if isinstance(content, str):
                            first_user = content
                        else:
                            parts = [part.get('text', '') for part in content if isinstance(part, dict) and part.get('type') == 'text']
                            first_user = '\n'.join(parts)
        title = (name or first_user or '-').replace('\n', ' ').strip()
        if len(title) > 120:
            title = title[:117] + '...'
        rows.append((path.stat().st_mtime, str(path), cwd, title or '-'))
    except Exception:
        continue

for _, path_str, cwd, title in sorted(rows, reverse=True):
    print(f"{path_str}\t{cwd}\t{title}")
PY
)}")

    if (( ${#sessions[@]} == 0 )); then
        echo "No pi sessions found"
        return 1
    fi

    picker_opts+=(
        --layout=reverse
        --border=rounded
        --delimiter='\t'
        --with-nth=2,3
        --prompt='pi sessions > '
        --header='All sessions'
    )

    if (( $+commands[jq] )); then
        picker_opts+=(
            --preview='_pic_preview {1}'
            --preview-window='right:65%:wrap'
            --preview-label='Conversation'
        )
    fi

    if typeset -f __fzf_popup > /dev/null; then
        selection=$(printf '%s\n' "${sessions[@]}" | __fzf_popup "${picker_opts[@]}")
    else
        selection=$(printf '%s\n' "${sessions[@]}" | fzf "${picker_opts[@]}")
    fi

    [[ -z "$selection" ]] && return 0

    session_file=${selection%%$'\t'*}
    pi --session "$session_file" "$@"
}
