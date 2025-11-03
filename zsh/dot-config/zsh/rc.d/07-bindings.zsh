# CTRL-G CTRL-G
# sync history
function sync-history-widget() {
  fc -I
  fc -R
  zle reset-prompt
}
zle -N sync-history-widget
bindkey '^G^G' sync-history-widget

# fzf
# bindkey -M emacs '^O' fzf-history-widget

# CTRL-O
# atuin
bindkey -M emacs '^O' atuin-search

# search for line up/down
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
bindkey '^P' up-line-or-beginning-search
bindkey '^N' down-line-or-beginning-search
bindkey '^R' history-incremental-pattern-search-backward
bindkey '^S' history-incremental-pattern-search-forward
bindkey '^k' kill-line
# <S-Tab> to reverse in completion menu
bindkey '^[[Z' reverse-menu-complete
# make delete key work
bindkey '\e[3~' delete-char
# run command, but do not clear commandline
bindkey '^\' accept-and-hold

# home/end to move to beginning/end of line
bindkey '^[[F' end-of-line
bindkey '^[[H' beginning-of-line

# CTRL-X CTRL-E
# edit command in vim
autoload edit-command-line
zle -N edit-command-line
bindkey '^X^e' edit-command-line

# ALT-;
# copy earlier word in line.. VERY useful with M-.
autoload -Uz copy-earlier-word
zle -N copy-earlier-word
bindkey '^[;' copy-earlier-word

# ALT-U
# on main prompt: push current command to stack, and replace with previous command
# on continuation prompt: move all entered lines to main prompt
# Use ALT-G to re-insert command
bindkey '^[u' push-line-or-edit

# ALT-v
# describe next keybinding
bindkey '^[v' describe-key-briefly

# Combined abbr expansion + magic-space
# Expands abbreviations AND does history expansion (!!,  !$, etc.)
abbr-magic-space() {
    # First, try to expand abbreviation
    zle abbr-expand
    # Then do history expansion
    zle magic-space
}
zle -N abbr-magic-space
bindkey ' ' abbr-magic-space
