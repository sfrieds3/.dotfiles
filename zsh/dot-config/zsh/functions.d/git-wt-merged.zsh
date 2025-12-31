git-wt-merged() {
  setopt local_options no_aliases
  git rev-parse --git-dir >/dev/null 2>&1 || { echo "Not a git repo." >&2; return 1; }

  local git_common_dir target_branch
  git_common_dir=$(git rev-parse --path-format=absolute --git-common-dir 2>/dev/null) || return 1

  local remote="${GIT_WORKTREE_REMOTE:-origin}"
  target_branch="${1:-}"

  if [[ -z "$target_branch" ]]; then
    if git show-ref --verify --quiet "refs/remotes/$remote/develop"; then
      target_branch="$remote/develop"
    elif git show-ref --verify --quiet "refs/remotes/$remote/main"; then
      target_branch="$remote/main"
    elif git show-ref --verify --quiet "refs/remotes/$remote/master"; then
      target_branch="$remote/master"
    else
      echo "No $remote/develop, $remote/main, or $remote/master found." >&2
      return 1
    fi
  elif [[ "$target_branch" != */* ]]; then
    if git show-ref --verify --quiet "refs/remotes/$remote/$target_branch"; then
      target_branch="$remote/$target_branch"
    elif ! git show-ref --verify --quiet "refs/heads/$target_branch"; then
      echo "Branch '$target_branch' not found locally or on $remote." >&2
      return 1
    fi
  fi

  local worktree_output
  worktree_output=$(git worktree list --porcelain)

  local wt_path wt_branch line
  local target_branch_short="${target_branch#*/}"
  local -a merged=() stale=() unmerged=()

  while IFS= read -r line; do
    case "$line" in
      worktree\ *)
        wt_path="${line#worktree }"
        ;;
      branch\ *)
        wt_branch="${line#branch refs/heads/}"
        if [[ "$wt_branch" != "$target_branch_short" ]]; then
          local ahead=$(git -C "$git_common_dir" rev-list --count "$target_branch..$wt_branch" 2>/dev/null)
          local behind=$(git -C "$git_common_dir" rev-list --count "$wt_branch..$target_branch" 2>/dev/null)
          if [[ "$ahead" == "0" ]]; then
            if [[ "$behind" == "0" ]]; then
              merged+=("$wt_branch|$wt_path")
            else
              stale+=("$behind|$wt_branch|$wt_path")
            fi
          else
            unmerged+=("$ahead|$behind|$wt_branch|$wt_path")
          fi
        fi
        wt_path=""
        wt_branch=""
        ;;
    esac
  done <<< "$worktree_output"

  echo "Worktrees compared to '$target_branch':"
  echo ""

  if [[ ${#merged[@]} -gt 0 ]]; then
    echo "Merged (safe to remove):"
    local e1
    for e1 in "${merged[@]}"; do
      echo "  ✓ ${e1%%|*}"
      echo "    └─ ${e1#*|}"
    done
    echo ""
  fi

  if [[ ${#stale[@]} -gt 0 ]]; then
    echo "Stale (no changes, just behind):"
    local e2 rest2
    for e2 in "${stale[@]}"; do
      rest2="${e2#*|}"
      echo "  ○ ${rest2%%|*} (${e2%%|*} behind)"
      echo "    └─ ${rest2#*|}"
    done
    echo ""
  fi

  if [[ ${#unmerged[@]} -gt 0 ]]; then
    echo "Unmerged (has unique commits):"
    local e3 rest3 rest3b
    for e3 in "${unmerged[@]}"; do
      rest3="${e3#*|}"
      rest3b="${rest3#*|}"
      echo "  ● ${rest3b%%|*} (+${e3%%|*}, -${rest3%%|*})"
      echo "    └─ ${rest3b#*|}"
    done
    echo ""
  fi

  if [[ ${#merged[@]} -eq 0 && ${#stale[@]} -eq 0 && ${#unmerged[@]} -eq 0 ]]; then
    echo "  (none)"
  fi
}

alias gwm='git-wt-merged'
