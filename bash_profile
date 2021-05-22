# .bash_profile

# source .bashrc only if interactive session
test -t 0 && . ~/.bashrc

# set XDG_CONFIG_HOME if not already set
#if [[ -n $XDG_CONFIG_HOME ]]
#then
    export XDG_CONFIG_HOME="$HOME/.config"
#fi

# User specific environment and startup programs
if ! [[ "$PATH" =~ "$HOME/lib:$HOME/bin:$HOME/.local/bin:$XDG_CONFIG_HOME/bin:" ]]
then
    PATH="$HOME/lib:$HOME/bin:$HOME/.local/bin:$XDG_CONFIG_HOME/bin:$PATH"
fi
export PATH
