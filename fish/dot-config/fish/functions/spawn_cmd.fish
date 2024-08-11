function spawn_cmd --argument _cmd --description "Run command in background even if shell session ends"
    nohup _cmd >/dev/nu.. 2>&1 &
    disown
end
