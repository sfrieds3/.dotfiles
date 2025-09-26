# shell tools
abbr add cg='cargo'
abbr add make_nvim='make CMAKE_INSTALL_PREFIX=$HOME/bin/nvim.build install CMAKE_BUILD_TYPE=Release -j 8'
abbr add renovate-config-validator='npx --yes --package renovate -- renovate-config-validator'

abbr add --quiet ls="eza"
abbr add l="eza $EZA_PARAMS"
abbr add lg"eza --git-ignore $EZA_PARAMS"
abbr add ll="eza --all --header --long $EZA_PARAMS"
abbr add llm="eza --all --header --long --sort=modified $EZA_PARAMS"
abbr add la="eza -lbhHigUmuSa"
abbr add lx="eza -lbhHigUmuSa@"
abbr add lt="eza --tree $EZA_PARAMS"
abbr add --quiet tree="eza --tree $EZA_PARAMS --git-ignore"
abbr add treea="eza --tree $EZA_PARAMS"
abbr add ...='cd ../..'
abbr add ..='cd ..'

abbr add rgrep='grep -r'
abbr add nvimdiff='nvim -d'

# terminal helpers
abbr add listports='lsof -nP -iTCP -sTCP:LISTEN'
abbr add largefiles='du -ah . | sort -h | tail -n 20'
abbr add pubsshcopy='pbcopy < ~/.ssh/id_rsa.pub'
abbr add diskusage='df -h'

# update history
abbr add hupdate='fc -RI'

# git
abbr add g='git'
abbr add gs='git status'
abbr add gba="git branch -a | fzf-tmux -p | sed 's/^[ \t]*//' | xargs git switch --detach"
abbr add gb="git branch | fzf-tmux -p | sed 's/^[ \t]*//' | xargs git switch"
abbr add gap='git add --patch'
abbr add gcp='git checkout --patch'
abbr add gdo='git diff origin/$(git rev-parse --abbrev-ref HEAD)'
abbr add gdh='git diff origin/HEAD'
abbr add gbp='git checkout -'
abbr add gd='git diff'
abbr add gdw='git diff --word-diff'
abbr add gdc='git diff --cached'
abbr add gdu='git diff @{upstream}'
abbr add gia='git ls-files -o --exclude-standard | fzf --print0 -m | xargs -0 -t -o git add --intent-to-add'
abbr add gwip='git add -A; git rm $(git ls-files --deleted) 2> /dev/null; git commit --no-verify -m "[WIP]: $(date)"'
abbr add gswip='git stash push -m "[WIP: $(git rev-parse --abbrev-ref HEAD)]: $(date)"'

# docker
abbr add drb='docker run -it --rm --entrypoint bash'
abbr add dil='docker image ls'

# k8s
abbr add k='kubectl'
abbr add klo='kubectl logs'
abbr add ks='kubectx && kubens'
abbr add kc='kubectx'
abbr add kn='kubens'
abbr add kk='__kubectl_info'
abbr add knt='kubectl get nodes -o custom-columns=NAME:.metadata.name,TAINTS:.spec.taints --no-headers'
abbr add kg='kubectl get'
abbr add ke='kubectl explain'
abbr add kgpo='kubectl get pod'
abbr add kgpow='kubectl get pod --watch'
abbr add kgdep='kubectl get deployment'
abbr add kgdepw='kubectl get deployment --watch'
abbr add kgsvc='kubectl get service'
abbr add kgsvcw='kubectl get service --watch'
abbr add kgcm='kubectl get configmap'
abbr add kgev='kubectl get events --sort-by=".lastTimestamp"'
abbr add klo='kubectl logs --follow'
abbr add klop='kubectl logs --follow --previous'
abbr add klon='kubectl logs -f --namespace'
abbr add kd='kubectl describe'
abbr add kdpo='kubectl describe pod'
abbr add kddep='kubectl describe deployment'
abbr add kdsvc='kubectl describe service'
abbr add kdsvcw='kubectl describe service --watch'
abbr add kdcm='kubectl describe configmap'
abbr add kjoc='kubectl get jobs --field_selector=status.successful=1'

# helm
abbr add helmup='helm repo update'
abbr add helmsr='helm search repo'
abbr add helmsra='helm search repo --devel --versions'

# python
abbr add pc='pre-commit'
abbr add pcr='pre-commit run'
abbr add pcra='pre-commit run --all-files'
abbr add pdoc='python3 -m pydoc'
abbr add pydocbuiltin='python3 -m pydoc builtins'
abbr add uvxi='uvx --isolated'
abbr add uv-pip-install-requirements='fd --type f --extension txt --exact-depth 1 requirements --exec uv pip install -r {}'
abbr add uv-pytest='uvx pytest'
abbr add uv-pytest-cov='uvx coverage -- run -m pytest'
abbr add uv-pytest-cov-report='uvx coverage -- report -m'
abbr add uv-jupyterlab='uvx --with=jupyterlab jupyter lab'
abbr add uv-pudb='uv runx --with=pudb --with=pytest pytest --pdbcls pudb.debugger:Debugger --pdb --capture=no'
abbr add uv-tox='uv runx --with=tox --with=tox-uv --with=pytest tox'
abbr add set-pudb-breakpoint='export PYTHONBREAKPOINT="pudb.set_trace"'
abbr add unset-pudb-breakpoint='unset PYTHONBREAKPOINT'
abbr add uvw='uv tool run --with'

# mise
abbr add mx='mise exec'
abbr add mr='mise run'
