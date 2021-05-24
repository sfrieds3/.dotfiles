# .bash_profile

# source .bashrc only if interactive session
test -t 0 && . ~/.bashrc

# set XDG_CONFIG_HOME if not already set
if [ -n "$XDG_CONFIG_HOME" ]
then
    export XDG_CONFIG_HOME="$HOME/.config"
fi

if [ -n "$RIPGREP_CONFIG_PATH" ]
then 
    export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc
fi

# User specific environment and startup programs
if ! [[ "$PATH" =~ "$HOME/lib:$HOME/bin:$HOME/.local/bin:$XDG_CONFIG_HOME/lib-scwfri:" ]]
then
    PATH="$HOME/lib:$HOME/bin:$HOME/.local/bin:$XDG_CONFIG_HOME/lib-scwfri:$PATH"
fi
export PATH
