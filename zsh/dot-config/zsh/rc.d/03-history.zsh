# history
HISTSIZE=1000000
SAVEHIST=1000000

# setopt SHARE_HISTORY          # share history across multiple zsh sessions
setopt APPEND_HISTORY           # append to history
setopt INC_APPEND_HISTORY_TIME  # adds commands as they are typed, not at shell exit
setopt HIST_EXPIRE_DUPS_FIRST   # expire duplicates first
setopt HIST_IGNORE_DUPS         # do not store duplications
setopt HIST_IGNORE_ALL_DUPS     # delete old recorded entry if new entry is duplicate
setopt HIST_SAVE_NO_DUPS        # do not write duplicate entries to the history file
setopt HIST_FIND_NO_DUPS        # ignore duplicates when searching
setopt HIST_REDUCE_BLANKS       # removes blank lines from history
setopt EXTENDED_HISTORY         # Include more information about when the command was executed,
setopt HIST_IGNORE_SPACE        # Don't enter commands into history if they start with a space
setopt HIST_VERIFY              # do not execute immediately upon history expansion

# set histfile locations
ZSH_DATA_DIR=${ZSH_DATA_DIR:=$XDG_DATA_HOME/zsh}
export HISTFILE=$ZSH_DATA_DIR/zsh_history
export ALT_HISTFILE=$ZSH_DATA_DIR/.full_history
