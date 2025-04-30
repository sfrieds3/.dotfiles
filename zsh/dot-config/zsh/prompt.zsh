# set prompt
autoload -U promptinit && promptinit
zmodload zsh/datetime
autoload -U add-zsh-hook
autoload -Uz vcs_info
autoload -Uz async && async

# example configuration
zstyle ':sfrieds3:prompt:separator:*' icon ' ∷ '
zstyle ':sfrieds3:prompt:separator:*' color red
zstyle ':sfrieds3:prompt:git:branch-icon:*' color yellow
zstyle ':sfrieds3:prompt:git:branch:*' color magenta
zstyle ':sfrieds3:prompt:git:stashed:*' color blue
zstyle ':sfrieds3:prompt:git:stashed:*' icon ' ≡'
zstyle ':sfrieds3:prompt:git:staged:*' color green
zstyle ':sfrieds3:prompt:git:staged:*' icon ' ●'
zstyle ':sfrieds3:prompt:git:unstaged:*' color red
zstyle ':sfrieds3:prompt:git:unstaged:*' icon ' ●'
zstyle ':sfrieds3:prompt:git:untracked:*' color yellow
zstyle ':sfrieds3:prompt:git:untracked:*' icon ' ●'
zstyle ':sfrieds3:prompt:docker:*' icon ''
zstyle ':sfrieds3:prompt:docker:*' color blue
zstyle ':sfrieds3:prompt:python:*' icon ''
zstyle ':sfrieds3:prompt:python:*' color green
zstyle ':sfrieds3:prompt:kube:*' icon ''
zstyle ':sfrieds3:prompt:kube:*' color blue
zstyle ':sfrieds3:prompt:pwd:dir:*' color cyan
zstyle ':sfrieds3:prompt:pwd:*' color magneta
zstyle ':sfrieds3:prompt:jobs:*' color yellow
zstyle ':sfrieds3:prompt:character:success:*' icon '❯'
zstyle ':sfrieds3:prompt:character:success:*' color blue
zstyle ':sfrieds3:prompt:character:error:*' icon '❯'
zstyle ':sfrieds3:prompt:exectime:*' color yellow

# load zstyle configurations
zstyle -s ':sfrieds3:prompt:git:branch:*' color PROMPT_BRANCH_COLOR || PROMPT_BRANCH_COLOR=magenta
zstyle -s ':sfrieds3:prompt:git:stashed:*' color PROMT_STASHED_COLOR || PROMT_STASHED_COLOR=blue
zstyle -s ':sfrieds3:prompt:git:stashed:*' icon PROMPT_STASHED_ICON || PROMPT_STASHED_ICON=' ≡'
zstyle -s ':sfrieds3:prompt:git:staged:*' color PROMT_STAGED_COLOR || PROMT_STAGED_COLOR=green
zstyle -s ':sfrieds3:prompt:git:staged:*' icon PROMPT_STAGED_ICON || PROMPT_STAGED_ICON=' ●'
zstyle -s ':sfrieds3:prompt:git:unstaged:*' color PROMPT_UNSTAGED_COLOR || PROMPT_UNSTAGED_COLOR=red
zstyle -s ':sfrieds3:prompt:git:unstaged:*' icon PROMPT_UNSTAGED_ICON || PROMPT_UNSTAGED_ICON=' ●'
zstyle -s ':sfrieds3:prompt:git:untracked:*' color PROMPT_UNTRACKED_COLOR || PROMPT_UNTRACKED_COLOR=yellow
zstyle -s ':sfrieds3:prompt:git:untracked:*' icon PROMPT_UNTRACKED_ICON || PROMPT_UNTRACKED_ICON=' ●'
zstyle -s ':sfrieds3:prompt:python:*' color PROMPT_PYTHON_COLOR || PROMPT_PYTHON_COLOR=green
zstyle -s ':sfrieds3:prompt:python:*' icon PROMPT_PYTHON_ICON || PROMPT_PYTHON_ICON=''
zstyle -s ':sfrieds3:prompt:docker:*' color PROMPT_DOCKER_COLOR || PROMPT_DOCKER_COLOR=blue
zstyle -s ':sfrieds3:prompt:docker:*' icon PROMPT_DOCKER_ICON || PROMPT_DOCKER_ICON=''
zstyle -s ':sfrieds3:prompt:kube:*' color PROMPT_KUBE_COLOR || PROMPT_KUBE_COLOR=blue
zstyle -s ':sfrieds3:prompt:kube:*' icon PROMPT_KUBE_ICON || PROMPT_KUBE_ICON=''
zstyle -s ':sfrieds3:prompt:pwd:dir:*' color PROMPT_DIR_COLOR || PROMPT_DIR_COLOR=cyan
zstyle -s ':sfrieds3:prompt:pwd:*' color PROMPT_PWD_COLOR || PROMPT_PWD_COLOR=magenta
zstyle -s ':sfrieds3:prompt:jobs:*' color PROMPT_JOBS_COLOR || PROMPT_JOBS_COLOR=yellow
zstyle -s ':sfrieds3:prompt:character:success:*' color PROMPT_CHARACTER_COLOR || PROMPT_CHARACTER_COLOR='❯'
zstyle -s ':sfrieds3:prompt:character:success:*' icon PROMPT_CHARACTER_ICON || PROMPT_CHARACTER_ICON=blue
zstyle -s ':sfrieds3:prompt:character:error:*' color PROMPT_CHARACTER_ERROR_COLOR || PROMPT_CHARACTER_ERROR_COLOR=red
zstyle -s ':sfrieds3:prompt:character:error:*' icon PROMPT_CHARACTER_ERROR_ICON || PROMPT_CHARACTER_ERROR_ICON='!!'
zstyle -s ':sfrieds3:prompt:exectime:*' color PROMPT_EXEC_TIME_COLOR || PROMPT_EXEC_TIME_COLOR=yellow
zstyle -s ':sfrieds3:prompt:separator:*' icon PROMPT_SEPARATOR_ICON || PROMPT_SEPARATOR_ICON=' │ '
zstyle -s ':sfrieds3:prompt:separator:*' color PROMPT_SEPARATOR_COLOR || PROMPT_SEPARATOR_COLOR=red

# https://github.com/wincent/wincent/blob/main/aspects/dotfiles/files/.zshrc
# https://github.com/akinsho/dotfiles/blob/5e5d579742f0edcd63c5b6e6c210a95242a35feb/.config/zsh/.zshrc
# http://zsh.sourceforge.net/Doc/Release/User-Contributions.html
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' stagedstr "%F{$PROMPT_STAGED_COLOR}$PROMPT_STAGED_ICON%f"  # nf-fa-check (default 'S')
zstyle ':vcs_info:*' unstagedstr "%F{$PROMPT_UNSTAGED_COLOR}$PROMPT_UNSTAGED_ICON%f"  # nf-fa-close (default 'U')
zstyle ':vcs_info:*' use-simple true
zstyle ':vcs_info:git+set-message:*' hooks git-untracked git-remotebranch git-compare # git-stashed
zstyle ':vcs_info:git*:*' formats "%{$__DOTS[ITALIC_ON]%}%F{$PROMPT_BRANCH_ICON_COLOR}λ:%F{$PROMPT_BRANCH_COLOR}%b%{$__DOTS[ITALIC_OFF]%}%f%m%c%u%f"  # default ' (%s)-[%b]%c%u-'
zstyle ':vcs_info:git*:*' actionformats "%{$__DOTS[ITALIC_ON]%}(%F{$PROMPT_BRANCH_ICON_COLOR}λ:%F{$PROMPT_BRANCH_COLOR}%b|%a%{$__DOTS[ITALIC_OFF]%}%f%m%c%u%f"  # default ' (%s)-[%b|%a]%...c%u-'

function +vi-git-untracked() {
    emulate -L zsh
    if [[ -n $(git ls-files --directory --no-empty-directory --exclude-standard --others 2> /dev/null) ]]; then
        hook_com[unstaged]+="%F{$PROMPT_UNTRACKED_COLOR}$PROMPT_UNTRACKED_ICON%f"  # nf-fa-question
    fi
}

function +vi-git-stashed() {
    emulate -L zsh
    if [[ -n $(git rev-list --walk-reflogs --count refs/stash 2> /dev/null) ]]; then
        hook_com[unstaged]+="%F{$PROMT_STASHED_COLOR}$PROMPT_STASHED_ICON%f"  # ⚑
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

    if [ -n "$NVIM" ]; then
        _LVL=$(($SHLVL-2))
    fi

    echo "$(eval printf \"$PROMPT_CHARACTER_ICON%.0s\" {1..$_LVL})"
}

# __PROMPT_SUCCESS="│ "
# __PROMPT_SUCCESS="❱  "

# PROMPT='$prompt_newline%F{red}∷ 20%D %* ∷ %F{blue}$(__kubectl_prompt)%F{green}$(__python_venv)%F{green}$(__conda_env)%F{cyan}$(__python_path)%F{magenta}$(__node_version)%F{yellow}%{$__DOTS[ITALIC_ON]%}${cmd_exec_time} %{$__DOTS[ITALIC_OFF]%}$prompt_newline%F{green}${PWD/#$HOME/~} %(1j.[%j] .)%(?.%F{green}$__PROMPT_SUCCESS.%F{red}[$EXIT_CODE]$__PROMPT_ERROR)%f'
# PS1='$(__mark_prompt)$prompt_newline%F{$PROMPT_PYTHON_COLOR}$(__python_venv)$(__conda_env)%f%F{$PROMPT_DIR_COLOR}$(basename $PWD)%F{$PROMPT_JOBS_COLOR}%B%(1j. [%j] .) %b%(?.%F{$PROMPT_CHARACTER_COLOR}$(__prompt_characters).%F{$PROMPT_CHARACTER_ERROR_COLOR}$PROMPT_CHARACTER_ERROR_ICON)%f '
PS1='$(__mark_prompt)$prompt_newline%F{$PROMPT_DIR_COLOR}${PWD/#$HOME/~}%f$prompt_newline$(__prompt__docker_context)$(__prompt__python_venv)$(__prompt__conda_env)%F{$PROMPT_JOBS_COLOR}%B%(1j. [%j] .)%b%(?.%F{$PROMPT_CHARACTER_COLOR}$(__prompt_characters).%F{$PROMPT_CHARACTER_ERROR_COLOR}$PROMPT_CHARACTER_ERROR_ICON)%f '
PS2=' '

function __set_rprompt__precmd() {
    # RPROMPT="${vcs_info_msg_0_}%F{cyan}%f"
    RPROMPT="%F{$PROMPT_EXEC_TIME_COLOR}%{$__DOTS[ITALIC_ON]%}${cmd_exec_time}%{$__DOTS[ITALIC_OFF]%} ${vcs_info_msg_0_}%f"
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
