function starship_transient_prompt_func
    printf "\n"
    starship module directory
    starship module character
end

function starship_transient_rprompt_func
    starship module $argv cmd_duration
    starship module $argv status
    starship module time
end
