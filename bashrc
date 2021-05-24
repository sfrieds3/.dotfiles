# .bashrc
if [ -f $XDG_CONFIG_HOME/lib-scwfri/bash_colors.sh ]; then
    . $XDG_CONFIG_HOME/lib-scwfri/bash_colors.sh
fi

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

export TERM=xterm-256color

# disable C-s/C-q behavior
stty -ixon

fzf_exec=$(which fzf)
rg_exec=$(which rg)
if [ -x "$fzf_exec" ] && [ -x "$rg_exec" ]
then
    # f3
    bind -x '"\eOR":"fda"'
    # f4
    bind -x '"\eOS":"history | fzf"'
fi

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

export EDITOR='vim'
export LSCOLORS="ExGxBxDxCxEgEdxbxgxcxd"

# User specific aliases
if [ -f $HOME/.bash_aliases ]; then
    . $HOME/.bash_aliases
fi

# user specific functions
if [ -f $HOME/.bash_functions ]
then
    . $HOME/.bash_functions
fi

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

    PS1="[\[\e[33m\]\u@\h:\[\e[m\]\[\e[37m\]\w\[\e[31m\]\`__git_ps1\`\[\e[m\]]\n$ " 
fi

if [ -f $HOME/bin/fzf-key-bindings.bash ]
then
    . $HOME/bin/fzf-key-bindings.bash
fi
