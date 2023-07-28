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

  echo (set_color cyan)$KUBECTL_PROMPT_ICON" ($ctx$KUBECTL_PROMPT_SEPARATOR$ns)"(set_color normal)
end

function python_venv -d "Get python venv"
    if set -q VIRTUAL_ENV
        set_color green
        printf " ( "
        printf (path basename $VIRTUAL_ENV)
        printf ")"
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
        printf " (🅒  $conda_environment)"
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
        printf " ($node_icon $_node_version)"
    end
end

function fish_prompt --description 'Write out the prompt'
    set -l last_status $status
    printf "\n"
    set_color red
    printf (date +"%Y-%m-%d ")
    printf (date +"%H:%M:%S ")
    set_color normal

    # PWD
    set_color $fish_color_cwd
    printf (prompt_pwd)
    set_color normal

    # other status
    printf (kubectl_status)
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
