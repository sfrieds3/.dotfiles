bindkey -e

setopt INTERACTIVE_COMMENTS
setopt AUTO_CD
setopt CORRECT
setopt CORRECT_ALL
setopt PROMPT_SUBST

export EDITOR="vim"

autoload -U promptinit && promptinit
PROMPT='%F{117}%2~%f %# '

if [ -f $HOME/bin/git/contrib/completion/git-prompt.sh ]; then
    . $HOME/bin/git/contrib/completion/git-prompt.sh

    GIT_PS1_SHOWDIRTYSTATE=1
    GIT_PS1_SHOWSTASHSTATE=1
    GIT_PS1_SHOWUNTRACKEDFILES=1
    RPROMPT='%F{203}$(__git_ps1) %F{217}%n%F{217}@%F{217}%M'
fi

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
# share history across multiple zsh sessions
setopt SHARE_HISTORY
# append to history
setopt APPEND_HISTORY
# adds commands as they are typed, not at shell exit
setopt INC_APPEND_HISTORY
# expire duplicates first
setopt HIST_EXPIRE_DUPS_FIRST 
# do not store duplications
setopt HIST_IGNORE_DUPS
#ignore duplicates when searching
setopt HIST_FIND_NO_DUPS
# removes blank lines from history
setopt HIST_REDUCE_BLANKS

autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^[[A" up-line-or-beginning-search # Up
bindkey "^[[B" down-line-or-beginning-search # Down
bindkey "^k" up-line-or-beginning-search # Up
bindkey "^j" down-line-or-beginning-search # Down

# completion
autoload -Uz compinit
compinit

zstyle ':completion:*' completer _complete _ignored
zstyle :compinstall filename '$HOME/.zshrc'

autoload -Uz compinit
compinit
