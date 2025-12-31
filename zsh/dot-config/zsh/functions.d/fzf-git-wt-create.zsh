# --- helpers ---------------------------------------------------------------
__sanitize_branch_for_path() {
  local b="$1"
  print -r -- "${b//\//-}" | sed -E 's/[^A-Za-z0-9._-]+/-/g'
}
__next_worktree_dir() {
  local branch="$1" git_root repo base dir n=0
  git_root=$(git rev-parse --path-format=absolute --git-common-dir 2>/dev/null) || return 1
  repo=${git_root:t}
  base=${GIT_WORKTREE_BASE:-$git_root}
  mkdir -p -- "$base" 2>/dev/null || true
  local safe="$(__sanitize_branch_for_path "$branch")"
  dir="${base%/}/${safe}"
  while [[ -e "$dir" ]]; do (( n++ )); dir="${base%/}/${safe}-${n}"; done
  print -r -- "$dir"
}
__branch_exists_local()  { git show-ref --verify --quiet "refs/heads/$1"; }
__branch_exists_remote_on() { # remote, branch
  git show-ref --verify --quiet "refs/remotes/$1/$2"
}
__find_remote_for_branch() { # branch -> prints remote or empty
  git for-each-ref --format='%(refname:short)' "refs/remotes/*/$1" \
    | sed -E 's#^remotes/([^/]+)/.*$#\1#' | head -n1
}

# --- core ------------------------------------------------------------------
git-wt-create() {
  setopt local_options no_aliases err_return
  git rev-parse --git-dir >/dev/null 2>&1 || { echo "Not a git repo." >&2; return 1; }

  local git_common_dir
  git_common_dir=$(git rev-parse --path-format=absolute --git-common-dir 2>/dev/null) || return 1
  local main_worktree="${git_common_dir%/.git}"

  # opts: -r/--remote
  local opt_remote
  zparseopts -D -E r:=opt_remote -remote:=opt_remote
  local REMOTE="${GIT_WORKTREE_REMOTE:-origin}"
  [[ -n "$opt_remote" ]] && REMOTE="${opt_remote[-1]}"

  local branch="$1"
  if [[ -z "$branch" ]]; then
    # build branch list (local + all remotes; dedupe)
    local choices
    choices=$(
      {
        git for-each-ref --format='%(refname:short)' refs/heads
        git for-each-ref --format='%(refname:short)' refs/remotes | sed -E 's#^remotes/[^/]+/##'
      } | grep -vE '^(HEAD|origin/HEAD)$' | sort -u
    ) || return 1

    local fzf_result fzf_exit
    fzf_result=$(print -r -- "$choices" | fzf \
      --print-query \
      --prompt="branch > " \
      --header="Select branch (Enter=select match, Ctrl-T=use typed value)" \
      --bind='ctrl-t:clear-selection+accept' \
      --preview='git log --oneline --decorate -20 -- {1} 2>/dev/null || true' \
      --preview-window="right:50%" --reverse --border --ansi
    ) || fzf_exit=$?

    # Exit codes: 0=match selected, 1=no match but Enter pressed, 130=cancelled (Esc/Ctrl-C)
    [[ ${fzf_exit:-0} -eq 130 ]] && return 130

    # fzf with --print-query returns:
    #   - If match selected: query on line 1, selection on line 2 (exit 0)
    #   - If no match but Enter: just query on line 1 (exit 1)
    # We prefer the selection if present, otherwise use the query
    local lines; lines=("${(@f)fzf_result}")
    if [[ ${#lines[@]} -ge 2 ]]; then
      # Has selection, use it
      branch="${lines[2]}"
    else
      # No selection, use query
      branch="${lines[1]}"
    fi
  fi

  # allow remote/branch input; normalize to branch only (but only if remote exists)
  if [[ "$branch" == */* ]]; then
    local potential_remote="${branch%%/*}"
    if git remote | grep -qx "$potential_remote"; then
      branch="${branch#*/}"
    fi
  fi
  [[ -z "$branch" ]] && return 130

  local dir; dir="$(__next_worktree_dir "$branch")" || return 1

  if [[ -n "$1" ]]; then
    # explicit arg -> new branch at HEAD
    git -C "$main_worktree" worktree add -b "$branch" "$dir" >&2 || return 1
  else
    if __branch_exists_local "$branch"; then
      git -C "$main_worktree" worktree add "$dir" "$branch" >&2 || return 1
    else
      # prefer selected remote; fall back to any remote that has it
      local src_remote="$REMOTE"
      __branch_exists_remote_on "$REMOTE" "$branch" || src_remote="$(__find_remote_for_branch "$branch")"
      if [[ -n "$src_remote" ]]; then
        git -C "$main_worktree" worktree add -b "$branch" "$dir" "$src_remote/$branch" >&2 || return 1
      else
        # no remote has it -> create new at HEAD
        git -C "$main_worktree" worktree add -b "$branch" "$dir" >&2 || return 1
      fi
    fi
  fi

  print -r -- "$dir"
}

# --- ZLE widget ------------------------------------------------------------
git-wt-create-widget() {
  local typed="${LBUFFER##* }"
  local newdir
  if [[ -n "$typed" ]]; then
    newdir="$(git-wt-create "$typed")" || return
  else
    newdir="$(git-wt-create)" || return
  fi
  [[ -n "$newdir" && -d "$newdir" ]] || return
  BUFFER="cd ${newdir}"
  zle accept-line
}

zle -N git-wt-create-widget
bindkey '^g^n' git-wt-create-widget   # Ctrl-g, Ctrl-n
alias gwc='git-wt-create'
