# navigation
abbr -a -- - prevd
abbr -a -- = nextd
abbr -a -- .. 'cd ..'
abbr -a -- ... 'cd ../..'

# shell tools
abbr -a -- ls eza
abbr -a -- cg cargo
abbr -a -- g git
abbr -a -- rgrep 'grep -r'
abbr -a -- nvimdiff 'nvim -d'

# terminal helpers
abbr -a -- duh 'du -sh * .[^.]* 2>/dev/null | sort -h'
abbr -a -- dskspc 'sudo du -h --max-depth=1 | sort -h'
abbr -a -- cleardns 'sudo dscacheutil -flushcache && sudo killall -HUP mDNSResponder'
abbr -a -- whatsmyip 'python3 -c "from socket import gethostbyname, gethostname; print(gethostbyname(gethostname()))"'
abbr -a -- testnvim 'nvim --clean -u $HOME/dev/minimal_init.vim'
abbr -a -- testnvimlua 'nvim --clean -u $HOME/dev/minimal_init.lua'

# git
abbr -a -- gs 'git status'
abbr -a -- gr 'cd (git rev-parse --show-toplevel)'
abbr -a -- gwr 'cd (git rev-parse --path-format=absolute --git-common-dir | sed "s|/\\.git/worktrees/.*||")'
abbr -a -- gap 'git add --patch'
abbr -a -- gcp 'git checkout --patch'
abbr -a -- gdo 'git diff origin/(git rev-parse --abbrev-ref HEAD)'
abbr -a -- gdh 'git diff origin/HEAD'
abbr -a -- gbp 'git checkout -'
abbr -a -- gd 'git diff'
abbr -a -- gdw 'git diff --word-diff'
abbr -a -- gdc 'git diff --cached'
abbr -a -- gdu 'git diff @{upstream}'
abbr -a -- gb "git branch | fzf-tmux -p | sed 's/^[ \t]*//' | xargs git switch"
abbr -a -- gba "git branch -a | fzf-tmux -p | sed 's/^[ \t]*//' | xargs git switch --detach"
abbr -a -- glf 'git ls-files --exclude-standard --deduplicate | fzf | xargs git lf'
abbr -a -- gaf 'git ls-files -m -o --exclude-standard --deduplicate | fzf --print0 -m | xargs -0 -t -o git add'
abbr -a -- gapf 'git ls-files -m --exclude-standard --deduplicate | fzf --print0 -m | xargs -0 -t -o git add --patch'
abbr -a -- gcpf 'git ls-files -m -o --exclude-standard --deduplicate | fzf --print0 -m | xargs -0 -t -o git checkout --patch'
abbr -a -- gdf 'git ls-files -m -o --exclude-standard --deduplicate | fzf-tmux -p | xargs git diff'
abbr -a -- gia 'git ls-files -o --exclude-standard | fzf --print0 -m | xargs -0 -t -o git add --intent-to-add'
abbr -a -- gwip 'git add -A; git rm (git ls-files --deleted) 2>/dev/null; git commit --no-verify -m "[WIP]: (date)"'
abbr -a -- gswip 'git stash push -m "[WIP: (git rev-parse --abbrev-ref HEAD)]: (date)"'

# fzf helpers
abbr -a -- cdf 'cd (fd | fzf-tmux -p --print0 | xargs -0 dirname)'
abbr -a -- cdd 'cd (fd -t d | fzf-tmux -p)'
abbr -a -- ef 'fzf-tmux -p | xargs nvim'

# docker
abbr -a -- dk docker
abbr -a -- dkps 'docker ps'
abbr -a -- dkpsa 'docker ps -a'
abbr -a -- dki 'docker images'
abbr -a -- dkrm 'docker rm'
abbr -a -- dkrmi 'docker rmi'
abbr -a -- dkl 'docker logs'
abbr -a -- dklf 'docker logs -f'
abbr -a -- dc 'docker compose'
abbr -a -- dcu 'docker compose up'
abbr -a -- dcud 'docker compose up -d'
abbr -a -- dcd 'docker compose down'
abbr -a -- dcl 'docker compose logs'
abbr -a -- dclf 'docker compose logs -f'
abbr -a -- dcb 'docker compose build'
abbr -a -- dcps 'docker compose ps'
abbr -a -- dcr 'docker compose run'
abbr -a -- dce 'docker compose exec'
abbr -a -- dcrestart 'docker compose restart'
abbr -a -- dcstop 'docker compose stop'
abbr -a -- dcrm 'docker compose rm'
abbr -a -- dcp 'docker compose pull'

# kubernetes
abbr -a -- k kubectl
abbr -a -- ks 'kubectx && kubens'
abbr -a -- kc kubectx
abbr -a -- kn kubens
abbr -a -- kk __kubectl_info
abbr -a -- knt 'kubectl get nodes -o custom-columns=NAME:.metadata.name,TAINTS:.spec.taints --no-headers'
abbr -a -- kg 'kubectl get'
abbr -a -- ke 'kubectl explain'
abbr -a -- kgpo 'kubectl get pod'
abbr -a -- kgpow 'kubectl get pod --watch'
abbr -a -- kgdep 'kubectl get deployment'
abbr -a -- kgdepw 'kubectl get deployment --watch'
abbr -a -- kgsvc 'kubectl get service'
abbr -a -- kgsvcw 'kubectl get service --watch'
abbr -a -- kgcm 'kubectl get configmap'
abbr -a -- kgev 'kubectl get events --sort-by=".lastTimestamp"'
abbr -a -- klo 'kubectl logs --follow'
abbr -a -- klop 'kubectl logs --follow --previous'
abbr -a -- klon 'kubectl logs -f --namespace'
abbr -a -- kd 'kubectl describe'
abbr -a -- kdpo 'kubectl describe pod'
abbr -a -- kddep 'kubectl describe deployment'
abbr -a -- kdsvc 'kubectl describe service'
abbr -a -- kdsvcw 'kubectl describe service --watch'
abbr -a -- kdcm 'kubectl describe configmap'
abbr -a -- kjoc 'kubectl get jobs --field_selector=status.successful=1'
abbr -a -- stern 'stern --tail 100'

# helm
abbr -a -- h helm
abbr -a -- hi 'helm install'
abbr -a -- hu 'helm upgrade'

# python
abbr -a -- pdoc 'python3 -m pydoc'
abbr -a -- pydocbuiltin 'python3 -m pydoc builtins'
abbr -a -- uvxi 'uvx --isolated'
abbr -a -- uv-pip-install-requirements "fd --type f --extension txt --exact-depth 1 requirements --exec uv pip install -r {}"
abbr -a -- uv-pytest 'uvx pytest'
abbr -a -- uv-pytest-cov 'uvx coverage -- run -m pytest'
abbr -a -- uv-pytest-cov-report 'uvx coverage -- report -m'
abbr -a -- uv-jupyterlab 'uvx --with=jupyterlab jupyter lab'
abbr -a -- uv-pudb 'uv runx --with=pudb --with=pytest pytest --pdbcls pudb.debugger:Debugger --pdb --capture=no'
abbr -a -- uv-tox 'uv runx --with=tox --with=tox-uv --with=pytest tox'
abbr -a -- set-pudb-breakpoint 'set -x PYTHONBREAKPOINT "pudb.set_trace"'
abbr -a -- unset-pudb-breakpoint 'set -e PYTHONBREAKPOINT'
abbr -a -- uvw 'uv tool run --with'

# pre-commit
abbr -a -- pcr 'pre-commit run'

# mise
abbr -a -- mx 'mise exec'
abbr -a -- mr 'mise run'

# misc
abbr -a -- cargo-install-update "cargo install (cargo install --list | egrep '^[a-z0-9_-]+ v[0-9.]+:\$' | cut -f1 -d' ')"
