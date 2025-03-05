# aliases
# aliases for directory stack
alias d='dirs -v'
for index ({0..9}) alias "$index"="cd +${index}"; unset index

alias -- -="cd -"

alias ls="eza"
alias l="eza $EZA_PARAMS"
alias lg"eza --git-ignore $EZA_PARAMS"
alias ll="eza --all --header --long $EZA_PARAMS"
alias llm="eza --all --header --long --sort=modified $EZA_PARAMS"
alias la="eza -lbhHigUmuSa"
alias lx="eza -lbhHigUmuSa@"
alias lt="eza --tree $EZA_PARAMS"
alias tree="eza --tree $EZA_PARAMS"

alias gr='cd $(git rev-parse --show-toplevel)'

# alias grep='grep -HIn0 --exclude=tags --color=auto'
alias rgrep='grep -r'

alias duh='du -sh * .[^.]* 2> /dev/null | sort -h'

# handy cd aliases
alias ...='cd ../..'
alias ..='cd ..'

# disk space
alias dskspc='sudo du -h --max-depth=1 | sort -h'

# kubernetes
alias k='kubectl'
alias ks='kubectx && kubens'
alias kc='kubectx'
alias kn='kubens'
alias kk='__kubectl_info'

# pre-commit
alias pcr='pre-commit run'

# fuzzy kill tmux sessions
alias tk="echo 'kill session:'; for s in \$(tmux list-sessions | awk '{print \$1}' | sed s/:\$// | fzf); do tmux kill-session -t \$s; done;"

# git aliases
alias gs='git status'
alias gba="git branch -a | fzf-tmux -p | sed 's/^[ \t]*//' | xargs git switch --detach"
alias gb="git branch | fzf-tmux -p | sed 's/^[ \t]*//' | xargs git switch"
alias gap='git add --patch'
alias gcp='git checkout --patch'
alias gdo='git diff origin/$(git rev-parse --abbrev-ref HEAD)'
alias gdh='git diff origin/HEAD'
alias gbp='git checkout -'
alias gd='git diff'
alias gdw='git diff --word-diff'
alias gdc='git diff --cached'
alias gia='git ls-files -o --exclude-standard | fzf --print0 -m | xargs -0 -t -o git add --intent-to-add'
alias gwip='git add -A; git rm $(git ls-files --deleted) 2> /dev/null; git commit --no-verify -m "[WIP]: $(date)"'
alias gswip='git stash push -m "[WIP: $(git rev-parse --abbrev-ref HEAD)]: $(date)"'

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

# python
alias condata='conda activate data'
alias pdoc='python3 -m pydoc'
alias pydocbuiltin='python3 -m pydoc builtins'
alias uv-pip-install-requirements='fd --type f --extension txt --exact-depth 1 requirements --exec uv pip install -r {}'
alias uv-pytest='uvx pytest'
alias uv-pytest-cov='uvx coverage -- run -m pytest'
alias uv-pytest-cov-report='uvx coverage -- report -m'
alias uv-jupyterlab='uvx --with=jupyterlab jupyter lab'
alias uv-pudb='uv runx --with=pudb --with=pytest pytest --pdbcls pudb.debugger:Debugger --pdb --capture=no'
alias uv-tox='uv runx --with=tox --with=tox-uv --with=pytest tox'
alias set-pudb-breakpoint='export PYTHONBREAKPOINT="pudb.set_trace"'
alias unset-pudb-breakpoint='unset PYTHONBREAKPOINT'

# mise
alias mx='mise exec'
alias mr='mise run'

# other various aliases
alias cleardns='sudo dscacheutil -flushcache && sudo killall -HUP mDNSResponder'
alias whatsmyip='python3 -c "from socket import gethostbyname, gethostname; print(gethostbyname(gethostname()))"'

# nvim diff
alias nvimdiff='nvim -d'

# test nvim
alias testnvim='nvim --clean -u $HOME/dev/minimal_init.vim'
alias testnvimlua='nvim --clean -u $HOME/dev/minimal_init.lua'
