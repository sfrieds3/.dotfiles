function fish_prompt
    # echo -ne "\n"(set_color red)"∷" (date +"%Y-%m-%d") (date +"%H:%M:%S") "∷"(set_color normal)"$__prompt_statuses\n"(set_color $fish_color_cwd)(prompt_pwd)(set_color normal)"$_status"
end
