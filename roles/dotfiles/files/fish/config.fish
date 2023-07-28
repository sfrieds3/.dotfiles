if status is-interactive
  set -gx fish_prompt_pwd_dir_length 3
  set -gx fish_prompt_pwd_full_dirs 3
end

# aliases
alias gap="git add --patch"

# pyenv init
if command -v ~/.pyenv/bin/pyenv 1>/dev/null 2>&1
  ~/.pyenv/bin/pyenv init - | source
end

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
if test -f /opt/homebrew/Caskroom/miniconda/base/bin/conda
    eval /opt/homebrew/Caskroom/miniconda/base/bin/conda "shell.fish" "hook" $argv | source
end
# <<< conda initialize <<<

# do not add conda env to prompt
function __conda_add_prompt
end
