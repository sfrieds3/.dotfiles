function fish_greeting --description "Fish greeting config"
    set -l greet \n(printf "Welcome to fish, the friendly interactive shell")\n
    set -l usern $USER
    set -l hostn (hostname)
    set -l dt (set_color yellow; date +"%Y-%m-%d"; set_color normal)
    set -l tm (set_color yellow; date +"%H:%M:%S"; set_color normal)

    echo "$greet$usern@$hostn - $dt $tm"
end
