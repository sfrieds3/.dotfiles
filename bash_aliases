# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

alias gg='$HOME/bin/git/git_recent.sh'
alias gl='git log --graph --oneline --decorate --all'
alias glg="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative"
alias gb='git rev-parse --abbrev-ref HEAD'
alias gs='git status -sbu'
alias b='git branch'
alias cdr='cd $(git rev-parse --show-toplevel)'

alias s='echo "$USER@$HOSTNAME"'
alias f="stat -c '%A %a %n'"

alias grep='grep -PIn --exclude=tags --color=auto'
alias tmux='tmux -2'

# du -sh
alias duh='du -sh -h * .[^.]* 2> /dev/null | sort -h'
