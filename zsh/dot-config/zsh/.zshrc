# zmodload zsh/zprof

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

# Create a hash table for globally stashing variables without polluting main
# scope with a bunch of identifiers.
typeset -A __DOTS

__DOTS[ITALIC_ON]=$'\e[3m'
__DOTS[ITALIC_OFF]=$'\e[23m'

# we will set our own title
DISABLE_AUTO_TITLE="true"

# auto cd dirs
# cdpath=($HOME/code $HOME/vault $HOME/.config $HOME/.dotfiles)

autoload -U colors && colors
autoload -U add-zsh-hook

# https://github.com/zsh-users/zsh/blob/8e1c6ed6bf416e7716b4c6d5c6822ec752db7b36/Functions/Misc/zmv
autoload zmv

# source plugins
source $ZDOTDIR/plugins/fzf-git.sh/fzf-git.sh
source $ZDOTDIR/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source $ZDOTDIR/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source $ZDOTDIR/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
source $ZDOTDIR/plugins/zsh-async/async.zsh

# fzf
source $ZDOTDIR/plugins/fzf/shell/completion.zsh
source $ZDOTDIR/plugins/fzf/shell/key-bindings.zsh

# custom completions
fpath=(
    $ZDOTDIR/completions
    $ZDOTDIR/plugins/zsh-completions/src
    /usr/local/share/zsh/site-functions
    $HOMEBREW_PREFIX/share/zsh/site-functions
    $fpath
)

# zoxide
eval "$(zoxide init zsh)"

# mise
eval "$(mise activate zsh)"
export MISE_ENV_FILE=.env

# atuin
export ATUIN_NOBIND="true"
eval "$(atuin init zsh)"

source $ZDOTDIR/functions.zsh
source $ZDOTDIR/completion.zsh
source $ZDOTDIR/bindings.zsh
source $ZDOTDIR/prompt.zsh
source $ZDOTDIR/aliases.zsh

# zsh-abbr
export ABBR_USER_ABBREVIATIONS_FILE=$XDG_DATA_HOME/zsh/abbr.zsh
export ABBR_QUIET=1
export ABBR_QUIETER=1
export ABBR_FORCE=1
export ABBR_SET_EXPANSION_CURSOR=1
source $ZDOTDIR/plugins/zsh-abbr/zsh-abbr.zsh
source $ZDOTDIR/plugins/zsh-autosuggestions-abbreviations-strategy/zsh-autosuggestions-abbreviations-strategy.zsh
ZSH_AUTOSUGGEST_STRATEGY=( abbreviations $ZSH_AUTOSUGGEST_STRATEGY )

function update_abbrs() {
    source $ZDOTDIR/abbr.zsh
}

# easily cd to directories (e.g. ~dot for ~/.dotfiles)
# hash -d dot="$HOME/.dotfiles"
# hash -d code="$HOME/code"
# hash -d vault="$HOME/vault"
# hash -d nvim="$XDG_CONFIG_HOME/nvim"
# hash -d zsh="$XDG_CONFIG_HOME/zsh"

# nvim as editor
export EDITOR="nvim"

foreach localfile (/etc/zsh/local ~/.zsh_local ~/.zshrc.local ~/.zshrc.$HOST ~/.zshrc.$USER); do
    if [[ -r $localfile ]]; then; echo "Sourcing $localfile"; source $localfile; fi
done

# ghostty shell integration
if [[ -n $GHOSTTY_RESOURCES_DIR ]]; then
    autoload -Uz -- "$GHOSTTY_RESOURCES_DIR"/shell-integration/zsh/ghostty-integration
    ghostty-integration
    unfunction ghostty-integration
fi

# >>> conda initialize >>>
__conda_setup="$('/opt/homebrew/Caskroom/miniconda/base/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/opt/homebrew/Caskroom/miniconda/base/etc/profile.d/conda.sh" ]; then
        . "/opt/homebrew/Caskroom/miniconda/base/etc/profile.d/conda.sh"
    else
        export PATH="/opt/homebrew/Caskroom/miniconda/base/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

typeset -U path PATH
typeset -U fpath

# zprof
