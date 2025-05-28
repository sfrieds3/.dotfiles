function fish_prompt
    echo -ne "\n"(set_color $fish_color_cwd)(prompt_pwd)"\n"(set_color normal)"$__prompt_statuses$_status"
end
