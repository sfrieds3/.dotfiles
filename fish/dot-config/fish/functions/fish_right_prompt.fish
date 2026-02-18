function fish_right_prompt -d "Right prompt with exec time, extras, git"
    set -l vcs (fish_vcs_prompt 2>/dev/null)

    set -l duration_str ""
    if set -q CMD_DURATION; and test $CMD_DURATION -ge 1000
        set duration_str (set_color yellow --italics)(__human_time $CMD_DURATION)(set_color normal)
    end

    set -l parts
    test -n "$duration_str"; and set -a parts $duration_str
    if set -q __rprompt_extras; and test -n "$__rprompt_extras"
        set -a parts $__rprompt_extras
    end
    test -n "$vcs"; and set -a parts $vcs

    string join " " -- $parts
end

function fish_right_prompt_loading_indicator -a last_prompt
    echo -n "$last_prompt" | sed -r 's/\x1B\[[0-9;]*[JKmsu]//g' | read -zl uncolored_last_prompt
    echo -n (set_color brblack)"$uncolored_last_prompt"(set_color normal)
end
