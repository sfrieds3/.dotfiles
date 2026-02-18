function fish_prompt
    set -l jobs_str ""
    set -l njobs (count (jobs -p 2>/dev/null))
    if test $njobs -gt 0
        set jobs_str (set_color yellow --bold)" [$njobs] "(set_color normal)
    end

    echo -ne "\n"(set_color $fish_color_cwd)(prompt_pwd)"\n"(set_color normal)"$__prompt_statuses$jobs_str$_status"
end
