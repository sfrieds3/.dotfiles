# emacs keybindings
bindkey -e

# Create a hash table for globally stashing variables without polluting main
# scope with a bunch of identifiers.
typeset -A __DOTS

__DOTS[ITALIC_ON]=$'\e[3m'
__DOTS[ITALIC_OFF]=$'\e[23m'

# we will set our own title
DISABLE_AUTO_TITLE="true"

# auto cd dirs
# cdpath=($HOME/code $HOME/vault $HOME/.config $HOME/.dotfiles)

# easily cd to directories (e.g. ~dot for ~/.dotfiles)
# hash -d dot="$HOME/.dotfiles"
# hash -d code="$HOME/code"
# hash -d vault="$HOME/vault"
# hash -d nvim="$XDG_CONFIG_HOME/nvim"
# hash -d zsh="$XDG_CONFIG_HOME/zsh"
