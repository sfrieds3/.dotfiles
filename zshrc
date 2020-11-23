# use emacs (readline) keybindings
bindkey -e
# vim as editor
export EDITOR="vim"

# setops
setopt INTERACTIVE_COMMENTS # allow comments even in interactive shells
setopt AUTO_CD # assume cd if directory passed
setopt CORRECT # try to correct spelling of commands
setopt CORRECT_ALL # try to correct spelling of all arguments
setopt PROMPT_SUBST # allow expansions in prompt (needed for __git_ps1)

# keybindings
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^[[A" up-line-or-beginning-search # Up
bindkey "^[[B" down-line-or-beginning-search # Down
bindkey "^k" up-line-or-beginning-search
bindkey "^j" down-line-or-beginning-search

# set prompt
autoload -U promptinit && promptinit
PROMPT='%F{117}%2~%f %# '

# git branch and status on right prompt, if available
if [ -f $HOME/bin/git/contrib/completion/git-prompt.sh ]; then
    . $HOME/bin/git/contrib/completion/git-prompt.sh

    GIT_PS1_SHOWDIRTYSTATE=1
    GIT_PS1_SHOWSTASHSTATE=1
    GIT_PS1_SHOWUNTRACKEDFILES=1
    RPROMPT='%F{203}$(__git_ps1) %F{217}%n%F{217}@%F{217}%M'
fi

# source bash_aliases, if available
if [ -f $HOME/.bash_aliases ]; then
    source $HOME/.bash_aliases
fi

# set gnome terminal title to user@host:pwd
DISABLE_AUTO_TITLE="true"
function precmd () {
  window_title="\033]0;$USER@$HOSTNAME:$PWD\007"
  echo -ne "$window_title"
}

# history
HISTSIZE=100000
SAVEHIST=100000
HISTFILE=${ZDOTDIR:-$HOME}/.zsh_history
setopt SHARE_HISTORY # share history across multiple zsh sessions
setopt APPEND_HISTORY # append to history
setopt INC_APPEND_HISTORY # adds commands as they are typed, not at shell exit
setopt HIST_EXPIRE_DUPS_FIRST  # expire duplicates first
setopt HIST_IGNORE_DUPS # do not store duplications
setopt HIST_FIND_NO_DUPS #ignore duplicates when searching
setopt HIST_REDUCE_BLANKS # removes blank lines from history

# completion
autoload -Uz compinit
compinit

zstyle ':completion:*' completer _complete _ignored
zstyle :compinstall filename '$HOME/.zshrc'
