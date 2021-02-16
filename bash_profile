# .bash_profile

# source .bashrc only if interactive session
test -t 0 && . ~/.bashrc && stty -ixon

# User specific environment and startup programs
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

