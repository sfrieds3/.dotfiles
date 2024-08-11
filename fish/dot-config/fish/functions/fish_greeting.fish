function fish_greeting --description "Fish greeting config"
    set -l greet (set_color brred; echo "Welcome to fish, the friendly interactive shell"; set_color normal)
    set -l hostn (set_color green; hostname; set_color normal)
    set -l usrhost (set_color green; echo "$USER@$hostn"; set_color normal)
    set -l dt (set_color brblack; date +"%Y-%m-%d"; set_color normal)
    set -l tm (set_color brblack; date +"%H:%M:%S"; set_color normal)
    set -l os (set_color cyan; echo "OS:"; uname -ro; set_color normal)
    set -l uptm (set_color brblack; echo "Uptime:"; uptime | sed 's/^.*up //'; set_color normal)

    echo
    echo "$usrhost"
    echo "$dt$tm"
    echo "$uptm"
    echo
    echo "$greet"
end
