function histgrep -d "Search alternate history file with ripgrep"
    set -l n_lines 10
    if string match -rq '^[0-9]+$' -- $argv[1]
        set n_lines $argv[1]
        set -e argv[1]
    end
    rg $argv $ALT_HISTFILE | tail -n $n_lines
end
