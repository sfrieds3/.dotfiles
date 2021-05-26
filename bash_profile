# .bash_profile

# source .bashrc only if interactive session
test -t 0 && . ~/.bashrc

# set XDG_CONFIG_HOME if not already set
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:=$HOME/.config}

# User specific environment and startup programs
if ! [[ "$PATH" =~ "$HOME/lib:$HOME/bin:$HOME/.local/bin:$XDG_CONFIG_HOME/lib-scwfri:" ]]
then
    PATH="$HOME/lib:$HOME/bin:$HOME/.local/bin:$XDG_CONFIG_HOME/lib-scwfri:$PATH"
fi
export PATH
