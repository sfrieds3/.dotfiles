# emacs keybindings
bindkey -e

# sync history
function sync-history-widget() {
  fc -I
  fc -R
  zle reset-prompt
}
zle -N sync-history-widget
bindkey '^G' sync-history-widget

# fzf
# bindkey -M emacs '^O' fzf-history-widget

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

# allow ctrl-x ctrl-e to edit command in vim
autoload edit-command-line
zle -N edit-command-line
bindkey '^X^e' edit-command-line

# copy earlier word in line with alt-;.. VERY useful with M-.
autoload -Uz copy-earlier-word
zle -N copy-earlier-word
bindkey "^[;" copy-earlier-word

# do history expansion on space
bindkey ' ' magic-space
