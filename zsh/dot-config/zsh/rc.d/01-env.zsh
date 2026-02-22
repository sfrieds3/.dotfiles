# nvim as editor
export EDITOR="nvim"

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CONFIG_DIRS="/etc/xdg"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_DATA_DIRS="/usr/local/share/:/usr/share/"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_RUNTIME_DIR="$TMPDIR"
export ZSH_DATA_DIR="$XDG_DATA_HOME/zsh"
export ZSH_CACHE_DIR="$XDG_CACHE_HOME/zsh"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export POETRY_DATA_DIR="$XDG_CONFIG_HOME/pypoetry"

# create uv venv with pip/setuptools/wheel
export UV_VENV_SEED=1

if [ -f "$HOME/.cargo/env" ]; then
    . "$HOME/.cargo/env"
fi

# ocaml
[[ ! -r "$HOME/.opam/opam-init/init.zsh" ]] || source "$HOME/.opam/opam-init/init.zsh" > /dev/null 2> /dev/null

export GOPATH="$HOME/go"
export GOBIN="$GOPATH/bin"

if [ ! -d $ZSH_DATA_DIR ]; then
    mkdir $ZSH_DATA_DIR
fi

if [ ! -d $ZSH_CACHE_DIR ]; then
    mkdir $ZSH_CACHE_DIR
fi
