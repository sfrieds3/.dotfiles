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
    ${HOMEBREW_PREFIX:-/opt/homebrew}/share/zsh/site-functions
    $fpath
)

# zoxide
eval "$(zoxide init zsh)"

# mise
export MISE_ENV_FILE=.env

function __lazy_init_mise() {
    if [[ -n "$__MISE_READY" ]]; then
        return 0
    fi

    eval "$(command mise activate zsh)"
    __MISE_READY=1
}

function mise() {
    __lazy_init_mise
    command mise "$@"
}

# atuin
export ATUIN_NOBIND="true"

function __lazy_init_atuin() {
    if [[ -n "$__ATUIN_READY" ]]; then
        return 0
    fi

    eval "$(command atuin init zsh)"
    __ATUIN_READY=1
}


# zsh-abbr
export ABBR_USER_ABBREVIATIONS_FILE=$XDG_DATA_HOME/zsh/abbr.zsh
export ABBR_QUIET=1
export ABBR_QUIETER=1
export ABBR_FORCE=1
export ABBR_SET_EXPANSION_CURSOR=1
export ABBR_SET_LINE_CURSOR=1
source $ZDOTDIR/plugins/zsh-abbr/zsh-abbr.zsh
source $ZDOTDIR/plugins/zsh-autosuggestions-abbreviations-strategy/zsh-autosuggestions-abbreviations-strategy.zsh
ZSH_AUTOSUGGEST_STRATEGY=( abbreviations $ZSH_AUTOSUGGEST_STRATEGY )

function update_abbrs() {
    source $ZDOTDIR/abbr.zsh
}
