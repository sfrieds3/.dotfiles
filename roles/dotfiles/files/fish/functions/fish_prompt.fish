function __kubectl_status -d "Get k8s ctx/ns"
    [ -z "$KUBECTL_PROMPT_ICON" ]; and set -l KUBECTL_PROMPT_ICON "☸"
    [ -z "$KUBECTL_PROMPT_SEPARATOR" ]; and set -l KUBECTL_PROMPT_SEPARATOR /
    set -l config $KUBECONFIG
    set --local KUBECTL_PROMPT_ICON "kube:"
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
    [ -z $ns ]; and set -l ns default

    echo (set_color cyan)" ("$KUBECTL_PROMPT_ICON" $ctx$KUBECTL_PROMPT_SEPARATOR$ns)"(set_color normal)
end

function __python_venv -d "Get python venv"
    if set -q VIRTUAL_ENV
        set -l venv_icon 
        set -l venv_icon "py-venv:"
        set -l venv_location (string replace $HOME/ '' $VIRTUAL_ENV)
        set_color green
        printf " ($venv_icon $venv_location)"
        set_color normal
    end
end

function __pyvenv_version -d "Get pyenv version"
    if test -e .python-version
        set -l py_icon 🐍
        set -l py_icon "py:"
        set -l py_version (pyenv local)
        set_color red
        printf " ($py_icon $py_version)"
        set_color normal
    end
end

function __conda_env -d "Get conda env"
    if set -q CONDA_PREFIX
        set -l conda_icon 🅒
        set -l conda_icon "conda:"
        set -l conda_environment (basename $CONDA_PREFIX)
        set_color green
        printf " ($conda_icon $conda_environment)"
        set_color normal
    end
end

function __is_node_dir -d "Are we in a node dir?"
    set -l flist "package.json" ".node-version" ".nvmrc" node_modules "*.js" "*.mjs" "*.cjs" "*.ts" "*.mts" "*.cts"

    for file in $flist
        if test -e $file
            true
            return
        end
    end
    false
    return
end

function __node_version -d "Get node version"
    if __is_node_dir and (command -v node 2> /dev/null)
        set -l node_icon 
        set -l node_icon "node:"
        set -l _node_version (node --version)
        set_color magenta
        printf " ($node_icon $_node_version)"
        set_color normal
    end
end

function __docker_context -d "Get docker context"
    set -l docker_icon 🐳
    set -l docker_icon "docker:"
    set -l dockerfiles "docker-compose.yaml" "docker-compose.yml" Dockerfile "compose.yaml" "compose.yml"
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

function __ssh_prompt --description "TODO: use to determine username/host in ssh or container"
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
    printf "∷"
    set_color normal


    # other status
    printf (__kubectl_status)
    printf (__docker_context)
    printf (__python_venv)
    printf (__pyvenv_version)
    printf (__conda_env)
    printf (__node_version)

    printf "\n"

    # PWD
    set_color $fish_color_cwd
    printf (prompt_pwd)
    set_color normal

    if not test $last_status -eq 0
        set_color $fish_color_error
        printf " [$last_status]"
        # printf "  "
        printf " !! "
    else
        printf " = "
    end

    set_color normal
end
