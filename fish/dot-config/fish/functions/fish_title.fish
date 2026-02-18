function fish_title
    set -l cmd (status current-command)
    set -l pwd_str (prompt_pwd)
    printf "%s:%s" $cmd $pwd_str
end
