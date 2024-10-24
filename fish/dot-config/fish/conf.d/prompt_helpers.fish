function __kubectl_status -d "Get k8s ctx/ns"
    [ -z "$KUBECTL_PROMPT_ICON" ]; and set -l KUBECTL_PROMPT_ICON "☸"
    [ -z "$KUBECTL_PROMPT_SEPARATOR" ]; and set -l KUBECTL_PROMPT_SEPARATOR /
    set -l config $KUBECONFIG
    set --local KUBECTL_PROMPT_ICON "k8s:"
    set --local KUBECTL_PROMPT_ICON ☸
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

    echo (set_color blue)" ("$KUBECTL_PROMPT_ICON" $ctx$KUBECTL_PROMPT_SEPARATOR$ns)"(set_color normal)
end

function __python_venv -d "Get python venv"
    if set -q VIRTUAL_ENV
        set -l venv_icon py-venv
        set -l venv_icon 
        set -l venv_location (string replace $HOME/ '' $VIRTUAL_ENV)
        set_color green
        printf " ($venv_icon $venv_location)"
        set_color normal
    end
end

function __python_version -d "Get python version"
    if test -e .python-version
        set -l py_icon py
        set -l py_icon 🐍
        set -l py_version (asdf current python | awk '{print $2}')
        set_color green
        printf " ($py_icon $py_version)"
        set_color normal
    end
end

function __python_path -d "Get python path"
    if not set -q VIRTUAL_ENV; and not set -q CONDA_PREFIX
        set -l py_icon 
        set -l pyversion (python --version | sed 's/^Python //')
        set -l whichpy (which python)
        set -l venv_location (string replace $HOME/ '' $whichpy)
        set_color cyan
        echo " ($py_icon $venv_location [$pyversion])"
        set_color normal
    end

end

function __conda_env -d "Get conda env"
    if set -q CONDA_PREFIX
        set -l conda_icon 🅒
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
        set -l _node_version (asdf current nodejs | awk '{print $2}')
        set_color magenta
        printf " ($node_icon v$_node_version)"
        set_color normal
    end
end

function __docker_context -d "Get docker context"
    set -l docker_icon "docker:"
    set -l docker_icon 🐳
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

function __rust_version --description "Get rust toolchain version"
    set -l flist "Cargo.toml" "*rs"
    set -l rust_icon 🦀
    for file in $flist
        if test -e $file
            set -l _rust_version (rustc --version | awk '{print $2}')
            set_color red
            printf " ($rust_icon $_rust_version)"
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

function _prompt_status_postexec --on-event fish_postexec
    set --global _status " │ "
    set --local __last_status $pipestatus
    for code in $__last_status
        if test $code -ne 0
            set --global _status (echo (set_color $fish_color_error) "[$code] !! ")
            break
        end
    end
end

function _prompt_helpers --on-event fish_prompt
    set --local __prompt_kubectl_status (__kubectl_status)
    set --local __prompt_docker_context (__docker_context)
    set --local __prompt_python_venv (__python_venv)
    set --local __prompt_python_version (__python_version)
    set --local __prompt_python_path (__python_path)
    set --local __prompt_conda_env (__conda_env)
    set --local __prompt_node_version (__node_version)
    set --local __prompt_rust_version (__rust_version)

    set --global __prompt_statuses "$__prompt_kubectl_status$__prompt_docker_context$__prompt_python_venv$__prompt_python_version$__prompt_python_path$__prompt_conda_env$__prompt_node_version$__prompt_rust_version"

    set --query _status || set --global _status " │ "
end

function __get_prompt_pwd --on-variable PWD
    set --global __prompt_pwd (prompt_pwd)
end
