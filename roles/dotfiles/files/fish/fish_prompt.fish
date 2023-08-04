function kubectl_status -d "Get k8s ctx/ns"
    [ -z "$KUBECTL_PROMPT_ICON" ]; and set -l KUBECTL_PROMPT_ICON "☸"
    [ -z "$KUBECTL_PROMPT_SEPARATOR" ]; and set -l KUBECTL_PROMPT_SEPARATOR "/"
    set -l config $KUBECONFIG
    [ -z "$config" ]; and set -l config "$HOME/.kube/config"
    if [ ! -f $config ]
        echo (set_color red)" ("$KUBECTL_PROMPT_ICON" "(set_color white)"no config)"(set_color normal)
        return
    end

    set -l ctx (kubectl config current-context 2>/dev/null)
    if [ $status -ne 0 ]
        echo (set_color red)" ("$KUBECTL_PROMPT_ICON" no context)"(set_color normal)
        return
    end

    set -l ns (kubectl config view -o "jsonpath={.contexts[?(@.name==\"$ctx\")].context.namespace}")
    [ -z $ns ]; and set -l ns 'default'

    echo (set_color cyan)" ("$KUBECTL_PROMPT_ICON" $ctx$KUBECTL_PROMPT_SEPARATOR$ns)"(set_color normal)
end

function python_venv -d "Get python venv"
    if set -q VIRTUAL_ENV
        set -l venv_icon 
        set -l venv_location (string replace $HOME/ '' $VIRTUAL_ENV)
        set_color green
        printf " ($venv_icon $venv_location)"
        set_color normal
    end
end

function pyvenv_version -d "Get pyenv version"
    if test -e .python-version
        set -l py_version (python3 --version | sed "s/^[^ ]* //")
        set_color red
        printf " (🐍 $py_version)"
        set_color normal
    end
end

function conda_env -d "Get conda env"
    if set -q CONDA_PREFIX
        set -l conda_environment (basename $CONDA_PREFIX)
        set_color green
        printf " (🅒  $conda_environment)"
        set_color normal
    end
end

function node_dir -d "Are we in a node dir?"
    set -l flist "package.json" ".node-version" ".nvmrc" "node_modules" "*.js" "*.mjs" "*.cjs" "*.ts" "*.mts" "*.cts"

    for file in $flist
        if test -e $file
            true
            return
        end
    end
    false
    return
end

function node_version -d "Get node version"
    if node_dir and (command -v node 2> /dev/null)
        set -l node_icon 
        set -l _node_version (node --version)
        set_color magenta
        printf " ($node_icon $_node_version)"
        set_color normal
    end
end

function docker_context -d "Get docker context"
    set -l docker_icon 🐳
    set -l dockerfiles "docker-compose.yaml" "docker-compose.yml" "Dockerfile"
    for dockerfile in $dockerfiles
        if test -e $dockerfile
            set -l _docker_context (docker context show)
            set_color blue
            printf " ($docker_icon $_docker_context)"
            set_color normal
            return
        end
    end
end

function ssh_prompt --description "TODO: use to determine username/host in ssh or container"
    # Only show host if in SSH or container
    # Store this in a global variable because it's slow and unchanging
    if not set -q prompt_host
        set -g prompt_host ""
        if set -q SSH_TTY
            or begin
                command -sq systemd-detect-virt
                and systemd-detect-virt -q
            end
            set prompt_host $usercolor$USER$normal@(set_color $fish_color_host)$hostname$normal":"
        end
    end
end

function fish_prompt --description "Config prompt"
    set -l last_status $status
    printf "\n"
    set_color red
    printf "∷ "
    printf (date +"%Y-%m-%d ")
    printf (date +"%H:%M:%S ")
    printf "∷ "
    set_color normal

    # PWD
    set_color $fish_color_cwd
    printf (prompt_pwd)
    set_color normal

    if [ $last_status -ne 0 ]
        set_color $fish_color_error
        printf " [$last_status]"
        set_color normal
    end

    # other status
    printf (kubectl_status)
    printf (docker_context)
    printf (python_venv)
    printf (pyvenv_version)
    printf (conda_env)
    printf (node_version)

    printf "\n"

    if not test $last_status -eq 0
        set_color $fish_color_error
        printf " "
    else
        printf "〉"
    end

    set_color normal
end

function fish_right_prompt -d "Config right prompt"
    set -g __fish_git_prompt_showdirtystate 1
    set -g __fish_git_prompt_showuntrackedfiles 1
    set -g __fish_git_prompt_showupstream informative
    set -g __fish_git_prompt_showcolorhints 1
    set -g __fish_git_prompt_use_informative_chars 1
    set -g __fish_git_prompt_show_informative_status 1
    set -g __fish_git_prompt_showstashstate 1
    set -g __fish_git_prompt_describe_style branch

    set -l vcs (fish_vcs_prompt 2>/dev/null)

    # set -l d (set_color brgrey)(date "+%R")(set_color normal)

    set -l duration "$cmd_duration$CMD_DURATION"
    if test $duration -gt 100
        set duration (math $duration / 1000)s
    else
        set duration
    end

    set_color reset
    string join " " -- $venv $duration $vcs $d
end
