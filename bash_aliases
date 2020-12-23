# some more ls aliases
alias ls='ls --color=auto'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

alias gg='$HOME/bin/git/git_recent.sh'
alias cdr='cd $(git rev-parse --show-toplevel)'

alias s='echo "$USER@$HOSTNAME"'
alias f="stat -c '%A %a %n'"

alias grep='grep -PIn --exclude=tags --color=auto'
alias tmux='tmux -2'

# du -sh
alias duh='du -sh -h * .[^.]* 2> /dev/null | sort -h'

# handy cd aliases
alias ...='cd ../..'
alias ..='cd ..'
