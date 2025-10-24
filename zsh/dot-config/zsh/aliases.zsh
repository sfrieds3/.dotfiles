# aliases
alias gr='cd $(git rev-parse --show-toplevel)'
alias gwr='cd $(git rev-parse --path-format=absolute --git-common-dir | sed "s|/\.git/worktrees/.*||")'

# kubecolor
alias kubectl='kubecolor'

# disk space
alias dskspc='sudo du -h --max-depth=1 | sort -h'
alias duh='du -sh * .[^.]* 2> /dev/null | sort -h'

# fuzzy kill tmux sessions
alias tk="echo 'kill session:'; for s in \$(tmux list-sessions | awk '{print \$1}' | sed s/:\$// | fzf); do tmux kill-session -t \$s; done;"

# fzf helpers
alias glf='git ls-files --exclude-standard --deduplicate | fzf | xargs git lf'
alias cdf='cd $(fd | fzf-tmux -p --print0 | xargs -0 dirname)'
# alias cdd='cd $(fd -t d | fzf --preview="stat {}" --preview-label="[ Dir stat ]")'
alias cdd='cd $(fd -t d | fzf-tmux -p)'
alias ef='fzf-tmux -p | xargs nvim'
alias gaf='git ls-files -m -o --exclude-standard --deduplicate | fzf --print0 -m | xargs -0 -t -o git add'
alias gapf='git ls-files -m --exclude-standard --deduplicate | fzf --print0 -m | xargs -0 -t -o git add --patch'
alias gcpf='git ls-files -m -o --exclude-standard --deduplicate | fzf --print0 -m | xargs -0 -t -o git checkout --patch'
alias gdf='git ls-files -m -o --exclude-standard --deduplicate | fzf-tmux -p | xargs git diff'

# other various aliases
alias cleardns='sudo dscacheutil -flushcache && sudo killall -HUP mDNSResponder'
alias whatsmyip='python3 -c "from socket import gethostbyname, gethostname; print(gethostbyname(gethostname()))"'

# update cargo install packages
alias cargo-install-update="cargo install $(cargo install --list | egrep '^[a-z0-9_-]+ v[0-9.]+:$' | cut -f1 -d' ')"

# test nvim
alias testnvim='nvim --clean -u $HOME/dev/minimal_init.vim'
alias testnvimlua='nvim --clean -u $HOME/dev/minimal_init.lua'
