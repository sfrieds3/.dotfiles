# .bashrc
# set XDG_CONFIG_HOME if not already set
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:=$HOME/.config}

[ -f $XDG_CONFIG_HOME/lib-scwfri/bash_colors.sh ] && . $XDG_CONFIG_HOME/lib-scwfri/bash_colors.sh

# Source global definitions
[ -f /etc/bashrc ] && . /etc/bashrc

# disable C-s/C-q behavior
stty -ixon

# user@host:pwd in titlebar
# update history with each read/write
# do this only when outside emacs, to allow for ansi-term to work
if [ -n "$INSIDE_EMACS" ]; then
    PROMPT_COMMAND='history -a; history -n'
else
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"; history -a; history -n'
fi

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000000
HISTFILESIZE=1000000
# write to / reload from history after every command
# this keeps command history in sync across shell sessions
export HISTCONTROL=erasedups
shopt -s histappend

set -o noclobber

export EDITOR='neovim'
export LSCOLORS="ExGxBxDxCxEgEdxbxgxcxd"

# User specific aliases
[ -f $HOME/.bash_aliases ] && . $HOME/.bash_aliases

# user specific functions
[ -f $HOME/.bash_functions ] && . $HOME/.bash_functions

# bash prompt setup
if [ -f /usr/share/bash-completion/completions/git ]
then
    . /usr/share/bash-completion/completions/git
elif [ -f /home/scwfri/.config/lib-scwfri/git/contrib/completion/git-completion.bash ]
then
    . /home/scwfri/.config/lib-scwfri/git/contrib/completion/git-completion.bash
fi

if [ -f $XDG_CONFIG_HOME/lib-scwfri/git/contrib/completion/git-prompt.sh ]
then
    . $XDG_CONFIG_HOME/lib-scwfri/git/contrib/completion/git-prompt.sh

    GIT_PS1_SHOWDIRTYSTATE=1
    GIT_PS1_SHOWSTASHSTATE=1
    GIT_PS1_SHOWUNTRACKEDFILES=1

    #PS1="[\[\e[33m\]\u@\h:\[\e[m\]\[\e[37m\]\w\[\e[31m\]\`__git_ps1\`\[\e[m\]]\n$ "
    PS1="\n\[\e[34m\]\u\[\e[m\]\[\e[37m\] at \[\e[m\]\[\e[33m\]\h\[\e[m\]\[\e[37m\] in \[\e[m\]\[\e[35m\]\w\[\e[m\]\[\e[31m\]\`__git_ps1\`\[\e[m\]\n→ "
fi

export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"

alias luamake=/home/scwfri/.cache/nvim/nlua/sumneko_lua/lua-language-server/3rd/luamake/luamake
