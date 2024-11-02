# completion
# format
# :completion:<function>:<completer>:<command>:<argument>:<tag>
# complist autoloaded before compinit
zmodload zsh/complist
zmodload -i zsh/stat
disable stat
autoload -Uz compinit && compinit -u

# complete hidden files/directories without requiring leading '.'
_comp_options+=(globdots)

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
zstyle ':completion:*' completer _expand_alias _extensions _complete _match _approximate

# complete options after `-`, not directory stack
zstyle ':completion:*' complete-options true

# use cache
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "$XDG_CACHE_HOME/zsh/.zcompcache"

# partial completion suggestions
zstyle ':completion:*' list-suffixes
zstyle ':completion:*' expand prefix suffix

# menu select options
zstyle ':completion:*' menu select
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:corrections' format ' %F{green}-- %d (errors: %e) --%f'
zstyle ':completion:*:descriptions' format ' %F{yellow}-- %d --%f'
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
zstyle ':completion:*' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*' format %F{yellow}%B%U%{$__DOTS[ITALIC_ON]%}%d%{$__DOTS[ITALIC_OFF]%}%b%u%f

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
