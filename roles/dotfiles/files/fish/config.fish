if status is-login
  # https://github.com/adrg/xdg/blob/master/README.md
  switch (uname)
  case "Darwin"
    set --global --export XDG_CONFIG_HOME $HOME/Library/Application Support
    set --global --export --path XDG_CONFIG_DIRS $HOME/Library/Preferences:/Library/Application Support:/Library/Preferences
    set --global --export XDG_DATA_HOME $HOME/Library/Application Support
    set --global --export XDG_DATA_DIRS /Library/Application Support
    set --global --export XDG_STATE_HOME $HOME/Library/Application Support
    set --global --export XDG_CACHE_HOME $HOME/Library/Caches
    set --global --export XDG_RUNTIME_DIR $TMPDIR
  case "*"
    set --global --export XDG_CONFIG_HOME $HOME/.config
    set --global --export XDG_CONFIG_DIRS /etc/xdg
    set --global --export XDG_DATA_HOME $HOME/.local/share
    set --global --export --path XDG_DATA_DIRS /usr/local/share/:/usr/share/
    set --global --export XDG_STATE_HOME $HOME/.local/state
    set --global --export XDG_CACHE_HOME $HOME/.cache
    set --global --export XDG_RUNTIME_DIR /run/user/$UID
  end

  # SET PATH
  set --global --export GOPATH $HOME/go
  set --global --export GOBIN $GOPATH/bin
  set LUAROCKSBIN $HOME/.luarocks/bin
  set PERL5BIN $HOME/perl5/bin
  set SYSGOBIN /usr/local/go/bin
  set RBENVBIN $HOME/.rbenv/bin
  set USRLOCALBIN /usr/local/bin
  set LOCALBIN $HOME/.local/bin
  set PYTHON3_VENV_BIN $HOME/.venv/venv/bin
  set OPENJDKBIIN /opt/homebrew/opt/openjdk/bin

  fish_add_path $LOCALBIN $USRLOCALBIN $GOBIN $SYSGOBIN $PERL5BIN $RBENVBIN $LUAROCKSBIN
  fish_add_path --append $PYTHON3_VENV_BIN $OPENJDKBIN
end

if status is-interactive
  set --global --export fish_prompt_pwd_dir_length 3
  set --global --export fish_prompt_pwd_full_dirs 3

  set fisher_path $__fish_config_dir/fisher
  set fish_function_path $fish_function_path $fisher_path/functions

  # git aliases
  alias gs='git status'
  alias gba='git branch -a | fzf-tmux -p | sed s/^\*// | xargs git switch'
  alias gb='git branch | fzf-tmux -p | sed s/^\*// | xargs git switch'
  alias gap='git add --patch'
  alias gcp='git checkout --patch'
  alias gdo='git diff origin/$(git rev-parse --abbrev-ref HEAD)'
  alias gdh='git diff origin/HEAD'
  alias gbp='git checkout -'
  alias gd='git diff'
  alias gdc='git diff --cached'

  # fzf aliaseg
  alias glf='git ls-files --exclude-standard | fzf | xargs git lf'
  alias cdf='cd $(fd | fzf-tmux -p --print0 | xargs -0 dirname)'
  alias cdd='cd $(fd -t d | fzf-tmux -p)'
  alias ef='fzf-tmux -p | xargs nvim'
  alias gaf='git ls-files -m -o --exclude-standard | fzf --print0 -m | xargs -0 -t -o git add'
  alias gapf='git ls-files -m -o --exclude-standard | fzf --print0 -m | xargs -0 -t -o git add --patch'
  alias gcpf='git ls-files -m -o --exclude-standard | fzf --print0 -m | xargs -0 -t -o git checkout --patch'
  alias gdf='git ls-files -m -o --exclude-standard | fzf-tmux -p | xargs git diff'

  # python
  alias condata="conda activate data"
  alias workonvenv="source $HOME/.venv/venv/bin/activate.fish"

  # pyenv init
  pyenv init - | source

  # >>> conda initialize >>>
  # !! Contents within this block are managed by 'conda init' !!
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

  # source our prompt
  source $__fish_config_dir/fish_prompt.fish
end
