# .bashrc

if [ -f $HOME/bin/bash_colors.sh ]; then
    . $HOME/bin/bash_colors.sh
fi

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi

export PATH
export PATH=${PATH}:/usr/local/bin

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=10000
HISTFILESIZE=20000
export HISTCONTROL=erasedups
export EDITOR='vim'
export LSCOLORS="ExGxBxDxCxEgEdxbxgxcxd"

# User specific aliases and functions
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# bash prompt setup
if [ -f $HOME/bin/git/contrib/completion/git-completion.bash ]; then
    . $HOME/bin/git/contrib/completion/git-completion.bash
fi

if [ -f $HOME/bin/git/contrib/completion/git-prompt.sh ]; then
    . $HOME/bin/git/contrib/completion/git-prompt.sh

    GIT_PS1_SHOWDIRTYSTATE=1
    GIT_PS1_SHOWSTASHSTATE=1
    GIT_PS1_SHOWUNTRACKEDFILES=1
    #PS1="[\[\e[33m\]\u\[\e[m\]\[\e[33m\]@\[\e[m\]\[\e[33m\]\h\[\e[m\]:\w\[\e[37m\]\`__git_ps1\`\[\e[m\]]\n$ "
    #PS1="\h:\W\$(__git_ps1) \u\$ "
    PS1="[\[\e[33m\]\h:\[\e[m\]\[\e[33m\]\W\[\e[31m\]\`__git_ps1\`\[\e[m\] \[\e[32m\]\u\[\e[m\]]$ "
fi

