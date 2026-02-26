#!/bin/zsh

function __fzf_popup() {
    if [[ -n "$TMUX" ]]; then
        fzf --tmux=90%,80%,border-native "$@"
    else
        fzf "$@"
    fi
}
