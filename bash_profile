# .bash_profile

# source .bashrc only if interactive session
test -t 0 && . ~/.bashrc

source ~/.bash_functions

USER_PATHS=(
    "$HOME/lib"
    "$HOME/bin"
    "$HOME/.local/bin"
    "$XDG_CONFIG_HOME/lib-scwfri"
    "$HOME/.rbenv/bin"
    "$HOME/.luarocks/bin"
)

export PATH=$(dedup "$PATH:$(join USER_PATHS[@])")

. "$HOME/.cargo/env"
eval "$(rbenv init - bash)"
