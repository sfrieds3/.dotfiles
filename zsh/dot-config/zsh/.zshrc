# setops
setopt   INTERACTIVE_COMMENTS # allow comments even in interactive shells
unsetopt AUTO_CD              # assume cd if directory passed
unsetopt CORRECT              # try to correct spelling of commands
unsetopt CORRECT_ALL          # try to correct spelling of all arguments
setopt   PROMPT_SUBST         # allow expansions in prompt (needed for __git_ps1)
setopt   COMPLETE_IN_WORD     # allow tab completion in the middle of a word
setopt   AUTO_PUSHD           # Push the current directory visited on the stack.
setopt   PUSHD_IGNORE_DUPS    # Do not store duplicates in the stack.
setopt   PUSHD_SILENT         # Do not print the directory stack after pushd or popd.
unsetopt FLOW_CONTROL         # disable C-s/C-q behavior
setopt   CHECK_JOBS           # check for running jobs on exit and warn (default behavior)
setopt   NO_HUP               # but do not send HUP signal to bg jobs on exit
setopt   HISTVERIFY           # show history expansions
setopt   IGNORE_EOF           # ignore EOF (but still close after 10 consecutive)
setopt   NO_CLOBBER           # prohibit 'cat foo > bar' if bar exists. use >! instead
setopt   EXTENDED_GLOB        # enables various things, most notably ^negation. '^', '#' and forgotwhich :/ see cheatsheet & http://zsh.dotsrc.org/Intro/intro_2.html#SEC2
setopt   NOTIFY               # [on] this will put info from finished background processes over the current line

# history
HISTSIZE=1000000000
SAVEHIST=1000000000
setopt SHARE_HISTORY           # share history across multiple zsh sessions
setopt APPEND_HISTORY          # append to history
setopt INC_APPEND_HISTORY      # adds commands as they are typed, not at shell exit
setopt HIST_EXPIRE_DUPS_FIRST  # expire duplicates first
setopt HIST_IGNORE_DUPS        # do not store duplications
setopt HIST_FIND_NO_DUPS       # ignore duplicates when searching
setopt HIST_REDUCE_BLANKS      # removes blank lines from history
setopt EXTENDED_HISTORY        # Include more information about when the command was executed,
setopt HIST_IGNORE_SPACE       # Don't enter commands into history if they start with a space
setopt HIST_VERIFY             # nicer substitution commands

# set histfile locations
ZSH_DATA_DIR=${ZSH_DATA_DIR:=$XDG_DATA_HOME/zsh}
export HISTFILE=$ZSH_DATA_DIR/zsh_history
export ALT_HISTFILE=$ZSH_DATA_DIR/.full_history

# Create a hash table for globally stashing variables without polluting main
# scope with a bunch of identifiers.
typeset -A __DOTS

__DOTS[ITALIC_ON]=$'\e[3m'
__DOTS[ITALIC_OFF]=$'\e[23m'

# we will set our own title
DISABLE_AUTO_TITLE="true"

# auto cd dirs
cdpath=($HOME/code $HOME/wiki $HOME/.config $HOME/.dotfiles)

autoload -U colors && colors
autoload -U add-zsh-hook

# https://github.com/zsh-users/zsh/blob/8e1c6ed6bf416e7716b4c6d5c6822ec752db7b36/Functions/Misc/zmv
autoload zmv

source $ZDOTDIR/functions.zsh
source $ZDOTDIR/completion.zsh
source $ZDOTDIR/bindings.zsh
source $ZDOTDIR/prompt.zsh
source $ZDOTDIR/aliases.zsh

# easily cd to directories (e.g. ~dot for ~/.dotfiles)
hash -d dot="$HOME/.dotfiles"
hash -d code="$HOME/code"
hash -d wiki="$HOME/wiki"
hash -d nvim="$XDG_CONFIG_HOME/nvim"
hash -d zsh="$XDG_CONFIG_HOME/zsh"

# nvim as editor
export EDITOR="nvim"

# kubernetes and helm completion
source <(kubectl completion zsh)
source <(helm completion zsh)

foreach localfile (/etc/zsh/local ~/.zsh_local ~/.zshrc.local ~/.zshrc.$HOST ~/.zshrc.$USER); do
    if [[ -r $localfile ]]; then; echo "Sourcing $localfile"; source $localfile; fi
done
