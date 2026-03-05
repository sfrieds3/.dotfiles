# completion
# format
# :completion:<function>:<completer>:<command>:<argument>:<tag>
# complist autoloaded before compinit
zmodload zsh/complist
zmodload zsh/datetime
zmodload zsh/stat

autoload -Uz compinit

# cache zcompdump
ZSH_COMPDUMP=${ZSH_COMPDUMP:-$XDG_CACHE_HOME/zcompdump}
mkdir -p "${ZSH_COMPDUMP:h}"

_compdump_max_age_seconds=$((7 * 24 * 60 * 60))
typeset -a _compdump_mtime

if [[ -s "$ZSH_COMPDUMP" ]] && zstat -A _compdump_mtime +mtime -- "$ZSH_COMPDUMP" 2> /dev/null && (( EPOCHSECONDS - _compdump_mtime[1] < _compdump_max_age_seconds )); then
    compinit -C -d "$ZSH_COMPDUMP"
else
    compinit -u -d "$ZSH_COMPDUMP"
fi

unset _compdump_max_age_seconds _compdump_mtime

autoload -Uz chpwd_recent_dirs cdr
add-zsh-hook chpwd chpwd_recent_dirs

# source dynamic completions
_completion_cache_dir="$XDG_CACHE_HOME/zsh/completions"
mkdir -p "$_completion_cache_dir"

_completion_cache_max_age_seconds=$((7 * 24 * 60 * 60))

_cache_completion() {
    local _name="$1"
    local _command="$2"
    local _cache_file="$_completion_cache_dir/${_name}.zsh"
    local _tmp_file="${_cache_file}.tmp"
    local _refresh=0
    local -a _cache_mtime

    if [[ ! -s "$_cache_file" ]]; then
        _refresh=1
    elif ! zstat -A _cache_mtime +mtime -- "$_cache_file" 2> /dev/null; then
        _refresh=1
    elif (( EPOCHSECONDS - _cache_mtime[1] >= _completion_cache_max_age_seconds )); then
        _refresh=1
    fi

    if (( _refresh )); then
        if eval "$_command" >| "$_tmp_file" 2> /dev/null; then
            mv "$_tmp_file" "$_cache_file"
        else
            rm -f "$_tmp_file"
        fi
    fi

    [[ -r "$_cache_file" ]] && source "$_cache_file"
}

_cache_completion kubectl "kubectl completion zsh"
compdef kubecolor=kubectl
_cache_completion helm "helm completion zsh"
_cache_completion docker "docker completion zsh"
_cache_completion uv "uv generate-shell-completion zsh"
_cache_completion stern "stern --completion=zsh"

if [[ -n "$HOMEBREW_PREFIX" ]]; then
    _gcloud_completion="$HOMEBREW_PREFIX/share/google-cloud-sdk/completion.zsh.inc"
    [[ -r "$_gcloud_completion" ]] && source "$_gcloud_completion"
fi

unset _gcloud_completion

## fzf-tab
source "${ZDOTDIR:-$HOME/.config/zsh}/plugins/fzf-tab/fzf-tab.plugin.zsh"

# Fix completions for uv run.
_uv_run_mod() {
    if [[ "$words[2]" == "run" && "$words[CURRENT]" != -* ]]; then
        _arguments '*:filename:_files -g "*.py"'
    else
        _uv "$@"
    fi
}
compdef _uv_run_mod uv

# complete hidden files/directories without requiring leading '.'
_comp_options+=(globdots)

# show group headers
zstyle ':fzf-tab:*' show-group full
zstyle ':fzf-tab:*' single-group color header
# disable sort when completing `git checkout`
zstyle ':completion:*:git-checkout:*' sort false
# preview directory's content with eza when completing cd
# zstyle ':fzf-tab:complete:cd:*' fzf-preview 'eza -1 --color=always $realpath'
# custom fzf flags
zstyle ':fzf-tab:*' fzf-flags --bind=tab:accept
# switch group using `<` and `>`
zstyle ':fzf-tab:*' switch-group '<' '>'
zstyle ':fzf-tab:*' fzf-command ftb-tmux-popup

zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:default' list-prompt '%S%M matches%s'

# complete ..<Tab> to parent dir
zstyle -e ':completion:*' special-dirs '[[ $PREFIX = (../)#(..) ]] && reply=(..)'

# show 20 matches at a time, insert in command line if less than 10
zstyle ':completion:*' file-list list=20 insert=0

# sort by date modified for cp and less
zstyle ':completion:*:*:cp:*' file-sort modification
zstyle ':completion:*:*:less:*' file-sort modification

# general completion
zstyle ':completion:*' completer _complete _ignored

# ignored completions
zstyle ':completion:*:ignored' format 'Ignored: %d'

# complete options after `-`, not directory stack
zstyle ':completion:*' complete-options true

# use cache
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "$XDG_CACHE_HOME/zsh/.zcompcache"

# partial completion suggestions
zstyle ':completion:*' list-suffixes
zstyle ':completion:*' expand prefix suffix

# menu select options
# zstyle ':completion:*' menu select
zstyle ':completion:*' menu no
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:corrections' format ' %F{green}-- %d (errors: %e) --%f'
zstyle ':completion:*:descriptions' format '[%d]'
# zstyle ':completion:*:descriptions' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*:messages' format ' %F{purple} -- %d --%f'
zstyle ':completion:*:warnings' format ' %F{red}-- no matches found --%f'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' verbose yes

# menu select keymaps
bindkey -M menuselect '^n' menu-complete
bindkey -M menuselect '^p' reverse-menu-complete
bindkey -M menuselect '\e' send-break
bindkey -M menuselect '^s' vi-insert  # toggle between normal and interactive
bindkey -M menuselect '^xg' clear-screen
bindkey -M menuselect '^xi' vi-insert
bindkey -M menuselect '^xh' accept-and-hold
bindkey -M menuselect '^xn' accept-and-infer-next-history
bindkey -M menuselect '^xu' undo

# format group names
# NOTE: fzf-tab ignores escape sequences in format, so use simple format
# zstyle ':completion:*' format ' %F{yellow}-- %d --%f'
# zstyle ':completion:*' format %F{yellow}%B%U%{$__DOTS[ITALIC_ON]%}%d%{$__DOTS[ITALIC_OFF]%}%b%u%f

# zstyle ':completion:*:*:cdr:*:*' menu select
# zstyle ':chpwd:*' recent-dirs-file ${ZSH_CACHE_DIR:=$XDG_CACHE_HOME/zsh}/.chpwd-recent-dirs +
# zstyle ':completion:*' recent-dirs-insert always
# zstyle ':chpwd:*' recent-dirs-default yes

# for cd, complete local directories, directories on stack, and directories in path
# zstyle ':completion:*:*:cd:*' tag-order local-directories directory-stack path-directories

# for commands, complete aliases, builtin, functions, then path commands
# zstyle ':completion:*:*:-command-:*:*' group-order aliases builtins functions commands

# when completing, keep the prefix around
zstyle ':completion:*' keep-prefix true

# - Try exact (case-sensitive) match first.
# - Then fall back to case-insensitive.
# - Accept abbreviations after . or _ or - (ie. f.b -> foo.bar).
# - Substring complete (ie. bar -> foobar).
zstyle ':completion:*' matcher-list '' '+m:{[:lower:]}={[:upper:]}' '+m:{[:upper:]}={[:lower:]}' '+m:{_-}={-_}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
