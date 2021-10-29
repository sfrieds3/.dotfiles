export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:=$HOME/.config}
export ZSH_CUSTOM=$HOME/.config/zsh/custom
export ZDOTDIR=$XDG_CONFIG_HOME/zsh

# set path and stuff
export PATH="$HOME/lib:$HOME/bin:$HOME/.local/bin:$XDG_CONFIG_HOME/lib-scwfri:$HOME/.luarocks/bin:$PATH"
typeset -U path

# set other env vars
export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"
export FZF_DEFAULT_COMMAND="rg --files"
export JDTLS_HOME="$HOME/bin/jdtls/"

fpath=($XDG_CONFIG_HOME/zsh/custom/themes $XDG_CONFIG_HOME/zsh $fpath)
