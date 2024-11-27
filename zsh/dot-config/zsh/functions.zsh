# ZSH only and most performant way to check existence of an executable
# https://www.topbug.net/blog/2016/10/11/speed-test-check-the-existence-of-a-command-in-bash-and-zsh/
function exists() { (( $+commands[$1] )); }

# reloads all functions
# http://www.zsh.org/mla/users/2002/msg00232.html
function r() {
    local f
    f=($XDG_CONFIG_HOME/functions.d/*(.))
    unfunction $f:t 2> /dev/null
    autoload -U $f:t
}

function __python_venv() {
    # [ $VIRTUAL_ENV ] && echo 'venv('`basename $VIRTUAL_ENV`') '
    # local __pyv=`python --version | sed 's/^Python //'`
    [[ -n $VIRTUAL_ENV ]] && echo "(${VIRTUAL_ENV:t}) "
}

function __conda_env() {
    if
        [ $CONDA_PREFIX ] && echo "(${CONDA_PREFIX:t}) "
}

function __node_dir() {
    # TODO this could be better done in a loop
    [ -f package.json ] || [ -f .node-version ] || [ -f .nvmrc ] || [ -f node_modules ] || [ -f *.js ] || [ -f *.mjs ] || [ -f *.cjs ] || [ -f *.ts ] || [ -f *.mts ] || [ -f *.cts ]
}

function __node_version() {
    __node_dir 2> /dev/null && command -v node > /dev/null && echo 'node('`node --version`') '
}

function __kubectl_prompt() {
    local __kube_ctx=$(kubectl config current-context 2> /dev/null)
    local __kube_ns=$(kubectl config view --minify --output 'jsonpath={..namespace}') 2> /dev/null
    # local __kube_user=$(kubectl config view --minify -o jsonpath='{.contexts[0].context.user}')
    # local __kube_ver=$(kubectl version 2>/dev/null | grep "Server Version" | sed 's/Server Version: \(.*\)/\1/')
    # echo "k8s($__kube_ver:$__kube_ctx/$__kube_ns) "
    echo "$PROMPT_KUBE_ICON $__kube_ctx/$__kube_ns "
}

function __pyenv_version() {
    [ -f .python-version ] 2> /dev/null && echo 'pyenv('`python3 --version | sed "s/^[^ ]* //"`') '
}

function __python_path() {
    local __pyv=`python --version | sed 's/^Python //'`
    local __pp=`which python`
    [[ -z $VIRTUAL_ENV ]] && echo "py(`relpath $__pp` [$__pyv]) "
}

# current status
function s() {
    (__kubectl_prompt && __python_path && __python_venv && __conda_env && __pyenv_version && __node_version) || return 0
}

function _update_title() {
  local a
  # escape '%' in $1, make nonprintables visible
  a=${(V)1//\%/\%\%}
  print -nz "%20>...>$a"
  read -rz a
  # remove newlines
  a=${a//$'\n'/}
  if [[ -n "$TMUX" ]] && [[ $TERM == screen* || $TERM == tmux* ]]; then
    print -n "\ek${(%)a}:${(%)2}\e\\"
  elif [[ "$TERM" =~ "screen*" ]]; then
    print -n "\ek${(%)a}:${(%)2}\e\\"
  elif [[ "$TERM" =~ "xterm*" || "$TERM" =~ "alacritty|wezterm" || "$TERM" =~ "st*" ]]; then
    print -n "\e]0;${(%)a}:${(%)2}\a"
  elif [[ "$TERM" =~ "^rxvt-unicode.*" ]]; then
    printf '\33]2;%s:%s\007' ${(%)a} ${(%)2}
  fi
}

# called just before the prompt is printed
function __zsh_title__precmd() {
  _update_title "zsh" "%20<...<%~"
}

# called just before a command is executed
function __zsh_title__preexec() {
  local -a cmd

  # Escape '\'
  1=${1//\\/\\\\\\\\}

  cmd=(${(z)1})             # Re-parse the command line

  # Construct a command that will output the desired job number.
  case $cmd[1] in
    fg)	cmd="${(z)jobtexts[${(Q)cmd[2]:-%+}]}" ;;
    %*)	cmd="${(z)jobtexts[${(Q)cmd[1]:-%+}]}" ;;
  esac
  _update_title "$cmd" "%20<...<%~"
}

function cursor_mode() {
    # See https://ttssh2.osdn.jp/manual/4/en/usage/tips/vim.html for cursor shapes
    cursor_block='\e[2 q'
    cursor_beam='\e[6 q'

    function zle-keymap-select {
        if [[ ${KEYMAP} == vicmd ]] ||
            [[ $1 = 'block' ]]; then
                echo -ne $cursor_block
            elif [[ ${KEYMAP} == main ]] ||
                [[ ${KEYMAP} == viins ]] ||
                [[ ${KEYMAP} = '' ]] ||
                [[ $1 = 'beam' ]]; then
                        echo -ne $cursor_beam
        fi
    }

    zle-line-init() {
        echo -ne $cursor_beam
    }

    zle -N zle-keymap-select
    zle -N zle-line-init
}

function relpath() {
    python -c "import os,sys;print(os.path.relpath(*(sys.argv[1:]), start=os.path.expanduser('~')))" "$@";
}

function histgrep() {
    local n_lines=10
    if [[ "$1" =~ ^[0-9]*$ ]]; then
        n_lines="$1"
        shift
    fi
    rg "$@" $ALT_HISTFILE | tail -n "$n_lines"
}

# quick switch to worktre
function cw() {
    cd $(git worktree list | fzf --prompt="Worktree: " --height 40% --reverse | awk '{print $1}') || return
}

function pbfilter() {
    if [ $# -gt 0 ]; then
        pbpaste | "$@" | pbcopy
    else
        pbpaste | pbcopy
    fi
}

function javaenv() {
    if [ $# -lt 1 ]
    then
        /usr/libexec/java_home -V
        return
    fi

    case "$1" in
        ls)
            /usr/libexec/java_home -V
            ;;
        set)
            _java_home=$(/usr/libexec/java_home -v $2)
            export JAVA_HOME=$_java_home
            echo "JAVA_HOME=$_java_home"
            ;;
        *)
            echo "Usage: $0 {ls|set [<version>]}"
            return 1
            ;;
    esac
}

function make_nvim() {
    if [ $# -gt 0 ]
    then
        make -j "$1" CMAKE_INSTALL_PREFIX=$HOME/bin/nvim.build install CMAKE_BUILD_TYPE=Release
        return
    fi

    make -j 8 CMAKE_INSTALL_PREFIX=$HOME/bin/nvim.build install CMAKE_BUILD_TYPE=Release
}

# convert timestamp
function tsconvert() {
    # Description: Convert unix timestamp to UTC time
    local timestamp="$1"

    if [[ -z "$timestamp" ]]; then
        echo "Usage: tsconvert <timestamp>"
        return 1
    fi

    local utc_time=$(python3 -c "
import datetime
timestamp = '$timestamp'.replace(',', '')
print(datetime.datetime.fromtimestamp(int(timestamp) / (1e3 if len(timestamp) > 10 else 1)).strftime('%Y-%m-%d %H:%M:%S UTC'))
    ")

    echo "$utc_time"
}

function venv() {
    # Description: Activate virtual environment in the current project, or create one if it doesn't exist
    local venv_dirs=(".venv" "venv")
    local conda_dirs=(".cenv" ".condaenv" "cenv" "condaenv")

    dir_exists() {
        [[ -d "$1" ]]
    }

    local current_dir="$(pwd)"

    local git_root
    git_root=$(git rev-parse --show-toplevel 2>/dev/null)

    echo "Attempting to activate a local venv..."

    for venv_dir in "${venv_dirs[@]}"; do
        if dir_exists "$current_dir/$venv_dir"; then
            echo "Activating virtual environment in $current_dir/$venv_dir"
            source "$current_dir/$venv_dir/bin/activate"
            return
        fi
    done

    if [[ -n "$git_root" ]]; then
        for venv_dir in "${venv_dirs[@]}"; do
            if dir_exists "$git_root/$venv_dir"; then
                echo "Activating virtual environment in $git_root/$venv_dir"
                source "$git_root/$venv_dir/bin/activate"
                return
            fi
        done
    fi

    echo "Did not find venv, attempting to activate a local conda env..."

    for conda_dir in "${conda_dirs[@]}"; do
        if dir_exists "$current_dir/$conda_dir"; then
            echo "Activating conda environment in $current_dir/$conda_dir"
            conda activate "$current_dir/$conda_dir"
            return
        fi
    done

    if [[ -n "$git_root" ]]; then
        for conda_dir in "${conda_dirs[@]}"; do
            if dir_exists "$git_root/$conda_dir"; then
                echo "Activating conda environment in $git_root/$conda_dir"
                conda activate "$git_root/$conda_dir"
                return
            fi
        done
    fi

    echo "Did not find a pip or conda environment to source, please create one first..."
}

function wezup() {
    brew upgrade --cask wezterm@nightly --no-quarantine --greedy-latest
}

function flushdns() {
    sudo dscacheutil -flushcache
    sudo killall -HUP mDNSResponder
}

function make_python() {
    cd "$HOME/code/lib/cpython"
    ./configure --enable-optimizations --prefix="$HOME/.local/bin/python"

  CFLAGS="-I$(brew --prefix openssl)/include" LDFLAGS="-L$(brew --prefix openssl)/lib" make
}
