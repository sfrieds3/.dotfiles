function fzf_search_history_insert --description 'Search command history, starting with an empty query. Insert an old command at the cursor. Adapted from _fzf_search_history'
    # history merge incorporates history changes from other fish sessions
    builtin history merge

    set command_with_ts (
        # Reference https://devhints.io/strftime to understand strftime format symbols
        builtin history --null --show-time="%m-%d %H:%M:%S │ " |
        _fzf_wrapper --read0 \
            --tiebreak=index \
            # preview current command using fish_ident in a window at the bottom 3 lines tall
            --preview="echo -- {4..} | fish_indent --ansi" \
            --preview-window="bottom:3:wrap" \
            $fzf_history_opts |
        string collect
    )

    if test $status -eq 0
        set command_selected (string split --max 1 " │ " $command_with_ts)[2]
        commandline --insert -- $command_selected
    end

    commandline --function repaint
end
