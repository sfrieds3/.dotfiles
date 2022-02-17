export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:=$HOME/.config}
export XDG_DATA_HOME=${XDG_DATA_HOME:=$XDG_CONFIG_HOME/local/share}
export XDG_CACHE_HOME=${XDG_CACHE_HOME:=$XDG_CONFIG_HOME/cache}
export ZDOTDIR=$XDG_CONFIG_HOME/zsh
export HISTFILE=$ZDOTDIR/zsh_history

# set path and stuff
export PATH=$HOME/lib:$HOME/bin:$HOME/.local/bin:$XDG_CONFIG_HOME/lib-scwfri:$HOME/.luarocks/bin:$PATH:/$HOME/perl5/bin
export PERL5LIB="/$HOME/perl5/lib/perl5"
export PERL_LOCAL_LIB_ROOT="/$HOME/perl5"
export PERL_MB_OPT="--install_base \"/$HOME/perl5\""
export PERL_MM_OPT="INSTALL_BASE=/$HOME/perl5"
typeset -U path

# set other env vars
export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc
export JDTLS_HOME=$HOME/bin/jdtls/
export FZF_DEFAULT_COMMAND="rg --files"

fpath=($XDG_CONFIG_HOME/zsh $fpath)
. "$HOME/.cargo/env"

if [[ $TERM =~ konsole.* ]]; then
    export FZF_DEFAULT_OPTS='--color fg+:5,hl+:6'
fi
