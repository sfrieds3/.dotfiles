# python alias
alias python='python3'

# eclipse alias
alias eclipse='/home/scott/eclipse/java-oxygen/eclipse/eclipse'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# neovim
alias vim='nvim'
alias vi='nvim'

# alias for sudo
# to check for aliases after sudo
alias sudo='sudo '

# alias for eclim
alias eclim='~/eclipse/java-latest-released/eclipse/eclimd'

# git aliases
alias gs='git status'
alias gd='git diff'
alias gca='git add -A && git commit'
alias gp='git pull'
alias gP='git push'
alias gc='git checkout'
alias gcb='git checkout branch'
alias gcm='git checkout master'
alias gm='git merge'
alias gbd='git branch -d'
alias gba='git branch -a'

# Dir aliases
alias class='cd ~/git/Class'
alias gogit='cd ~/go/src/github.com/sfrieds3'
alias noteit='go run ~/go/src/github.com/sfrieds3/noteit/main.go'
alias raftka='cd ~/git/raftka'


# Python pip alias - auto install for --user
function pip() {
  if [[ "$1" == "install" ]]; then
    shift 1
    command pip install --user "$@"
  else
    command pip "$@"
  fi
}
