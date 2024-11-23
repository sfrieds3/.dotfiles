# set prompt
autoload -U promptinit && promptinit
zmodload zsh/datetime
autoload -U add-zsh-hook
autoload -Uz vcs_info

# https://github.com/wincent/wincent/blob/main/aspects/dotfiles/files/.zshrc
# https://github.com/akinsho/dotfiles/blob/5e5d579742f0edcd63c5b6e6c210a95242a35feb/.config/zsh/.zshrc
# http://zsh.sourceforge.net/Doc/Release/User-Contributions.html
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' stagedstr "%F{green} ●%f"  # nf-fa-check (default 'S')
zstyle ':vcs_info:*' unstagedstr "%F{red} ✚%f"  # nf-fa-close (default 'U')
zstyle ':vcs_info:*' use-simple true
zstyle ':vcs_info:git+set-message:*' hooks git-untracked git-stashed git-compare git-remotebranch
zstyle ':vcs_info:git*:*' formats '%{$__DOTS[ITALIC_ON]%}%F{yellow}λ:%F{magenta}(%b)%{$__DOTS[ITALIC_OFF]%}%f%m%c%u%f%F{red}%f'  # default ' (%s)-[%b]%c%u-'
zstyle ':vcs_info:git*:*' actionformats '%{$__DOTS[ITALIC_ON]%}(%F{yellow}λ:%F{magenta}(%b|%a)%{$__DOTS[ITALIC_OFF]%}%f%m%c%u%f%F{red}%f'  # default ' (%s)-[%b|%a]%...c%u-'

function +vi-git-untracked() {
    emulate -L zsh
    if [[ -n $(git ls-files --directory --no-empty-directory --exclude-standard --others 2> /dev/null) ]]; then
        hook_com[unstaged]+="%F{red} …%f"  # nf-fa-question
    fi
}

function +vi-git-stashed() {
    emulate -L zsh
    if [[ -n $(git rev-list --walk-reflogs --count refs/stash 2> /dev/null) ]]; then
        hook_com[unstaged]+="%F{blue} ≡%f"  # ⚑
    fi
}

# git: Show +N/-N when your local branch is ahead-of or behind remote HEAD.
# Make sure you have added misc to your 'formats':  %m
# source: https://github.com/zsh-users/zsh/blob/545c42cdac25b73134a9577e3c0efa36d76b4091/Misc/vcs_info-examples#L180
function +vi-git-compare() {
    local ahead behind
    local -a gitstatus

    # Exit early in case the worktree is on a detached HEAD
    git rev-parse ${hook_com[branch]}@{upstream} >/dev/null 2>&1 || return 0

    local -a ahead_and_behind=(
    $(git rev-list --left-right --count HEAD...${hook_com[branch]}@{upstream} 2>/dev/null)
    )

    ahead=${ahead_and_behind[1]}
    behind=${ahead_and_behind[2]}

    local ahead_symbol="%{$fg[red]%}⇡%{$reset_color%}${ahead}"
    local behind_symbol="%{$fg[cyan]%}⇣%{$reset_color%}${behind}"
    (( $ahead )) && gitstatus+=( "${ahead_symbol}" )
    (( $behind )) && gitstatus+=( "${behind_symbol}" )
    hook_com[misc]+=${(j:/:)gitstatus}
}

## git: Show remote branch name for remote-tracking branches
function +vi-git-remotebranch() {
    local remote

    # Are we on a remote-tracking branch?
    remote=${$(git rev-parse --verify ${hook_com[branch]}@{upstream} \
        --symbolic-full-name 2>/dev/null)/refs\/remotes\/}

    # The first test will show a tracking branch whenever there is one. The
    # second test, however, will only show the remote branch's name if it
    # differs from the local one.
    # if [[ -n ${remote} ]] ; then
    if [[ -n ${remote} && ${remote#*/} != ${hook_com[branch]} ]]; then
        hook_com[branch]="${hook_com[branch]}→[${remote}]"
    fi
}

function __prompt__precmd() {
    vcs_info
    EXIT_CODE=$?
    echo "$(date +%Y-%m-%d--%H-%M-%S) $(hostname) $PWD $(history -1)" >> $ALT_HISTFILE
}
add-zsh-hook precmd __prompt__precmd

function __mark_prompt() {
    print -Pn "\e]133;A\007"
}


function __prompt_characters() {
    local IN_TMUX=$([[ "$TERM" =~ "tmux" ]] && echo tmux)
    local _LVL=$(($SHLVL))
    if [ -n "$IN_TMUX" ]; then
        _LVL=$(($SHLVL-1))
    fi

    echo "$(eval printf '❯%.0s' {1..$_LVL}) "
}

# __PROMPT_SUCCESS="│ "
# __PROMPT_SUCCESS="❱  "
local __PROMPT_SUCCESS="❯ "
local __PROMPT_ERROR="!! "

# PROMPT='$prompt_newline%F{red}∷ 20%D %* ∷ %F{blue}$(__kubectl_prompt)%F{green}$(__python_venv)%F{green}$(__conda_env)%F{cyan}$(__python_path)%F{magenta}$(__node_version)%F{yellow}%{$__DOTS[ITALIC_ON]%}${cmd_exec_time} %{$__DOTS[ITALIC_OFF]%}$prompt_newline%F{green}${PWD/#$HOME/~} %(1j.[%j] .)%(?.%F{green}$__PROMPT_SUCCESS.%F{red}[$EXIT_CODE]$__PROMPT_ERROR)%f'
PS1='$(__mark_prompt)$prompt_newline%F{green}$(__python_venv)$(__conda_env)%f%F{cyan}$(basename $PWD)%F{yellow}%B%(1j. [%j] .) %b%(?.%F{blue}$(__prompt_characters).%F{red}[$EXIT_CODE]$__PROMPT_ERROR)%f'
PS2=' '

function __set_rprompt__precmd() {
    # RPROMPT="${vcs_info_msg_0_}%F{cyan}%f"
    RPROMPT="%F{yellow}%{$__DOTS[ITALIC_ON]%}${cmd_exec_time}%{$__DOTS[ITALIC_OFF]%} %F{blue}$(__kubectl_prompt)%F{red}∷ ${vcs_info_msg_0_} %F{magenta}${PWD/#$HOME/~}%f"
}
add-zsh-hook precmd __set_rprompt__precmd


#-------------------------------------------------------------------------------
#           Execution time
#-------------------------------------------------------------------------------
# Inspired by https://github.com/sindresorhus/pure/blob/81dd496eb380aa051494f93fd99322ec796ec4c2/pure.zsh#L47
#
# Turns seconds into human readable time.
# 165392 => 1d 21h 56m 32s
# https://github.com/sindresorhus/pretty-time-zsh
function __human_time_to_var() {
    local human total_seconds=$1 var=$2
    local days=$(( total_seconds / 60 / 60 / 24 ))
    local hours=$(( total_seconds / 60 / 60 % 24 ))
    local minutes=$(( total_seconds / 60 % 60 ))
    local seconds=$(( total_seconds % 60 ))
    (( days > 0 )) && human+="${days}d "
    (( hours > 0 )) && human+="${hours}h "
    (( minutes > 0 )) && human+="${minutes}m "
    human+="${seconds}s"

  # Store human readable time in a variable as specified by the caller
  typeset -g "${var}"="${human}"
}

# Stores (into cmd_exec_time) the execution
# time of the last command if set threshold was exceeded.
function __check_cmd_exec_time() {
    integer elapsed
    (( elapsed = EPOCHSECONDS - ${cmd_timestamp:-$EPOCHSECONDS} ))
    typeset -g cmd_exec_time=
    (( elapsed > 1 )) && {
        __human_time_to_var $elapsed "cmd_exec_time"
    }
}

__timings__preexec() {
    emulate -L zsh
    typeset -g cmd_timestamp=$EPOCHSECONDS
}
add-zsh-hook preexec __timings__preexec

__timings__precmd() {
    __check_cmd_exec_time
    unset cmd_timestamp
}
add-zsh-hook precmd __timings__precmd
