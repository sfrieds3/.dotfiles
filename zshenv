export ZSH_CUSTOM=~/.config/zsh/custom
export XDG_CONFIG_HOME=~/.config
export ZDOTDIR=$XDG_CONFIG_HOME/zsh

fpath=($XDG_CONFIG_HOME/zsh/custom/themes $XDG_CONFIG_HOME/zsh $fpath)

if ! [[ "$PATH" =~ "$HOME/lib:$HOME/bin:$HOME/.local/bin:$XDG_CONFIG_HOME/lib-scwfri:" ]]
then
    PATH="$HOME/lib:$HOME/bin:$HOME/.local/bin:$XDG_CONFIG_HOME/lib-scwfri:$PATH"
fi
export PATH
