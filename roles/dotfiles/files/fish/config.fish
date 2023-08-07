if status is-login
end

if status is-interactive
  set --global --export fish_prompt_pwd_dir_length 3
  set --global --export fish_prompt_pwd_full_dirs 3

  # pyenv init
  pyenv init - | source

  # >>> conda initialize >>>
  if test -f /opt/homebrew/Caskroom/miniconda/base/bin/conda
    eval /opt/homebrew/Caskroom/miniconda/base/bin/conda "shell.fish" "hook" $argv | source
  end
  # <<< conda initialize <<<

  # do not add conda env to prompt
  function __conda_add_prompt
  end

  # load kubectl completions
  kubectl completion fish | source

  # load local config from ~/.fish_local, if available
  set -l local_config "$HOME/.fish_local"
  if test -e $local_config
    source $local_config
  end

  # configure fzf bindings
  fzf_configure_bindings --history=\co
end
