# fzf alias
# fuzzy find and open in nvim.. only if in interactive shell
test -t 0 && bind -x '"\C-g":fzn'

# bind ctrl-G to "directory up".. only if in interactive shell
test -t 0 && bind '"\C-q":"cd ..\C-m"'

# some more ls aliases
alias ls='ls --color=auto'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

alias gg='$HOME/bin/git/git_recent.sh'
alias cdr='cd $(git rev-parse --show-toplevel)'

alias s='echo "$USER@$HOSTNAME"'
alias f="stat -c '%A %a %n'"

alias grep='grep -HPIn --exclude=tags --color=auto'
alias tmux='tmux -2'

# du -sh
alias duh='du -sh * .[^.]* 2> /dev/null | sort -h'

# handy cd aliases
alias ...='cd ../..'
alias ..='cd ..'

# disk space
alias dskspc='sudo du -h --max-depth=1 | sort -h'
