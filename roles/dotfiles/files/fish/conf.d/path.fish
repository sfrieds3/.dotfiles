switch (uname)
case "Darwin"
  set --global --export XDG_CONFIG_HOME $HOME/.config
  set --global --export XDG_CONFIG_DIRS /etc/xdg
  set --global --export XDG_DATA_HOME $HOME/.local/share
  set --global --export --path XDG_DATA_DIRS /usr/local/share/:/usr/share/
  set --global --export XDG_STATE_HOME $HOME/.local/state
  set --global --export XDG_CACHE_HOME $HOME/.cache
  set --global --export XDG_RUNTIME_DIR $TMPDIR
  set --global --export HOMEBREW_PREFIX (brew --prefix)
case "*"
  set --global --export XDG_CONFIG_HOME $HOME/.config
  set --global --export XDG_CONFIG_DIRS /etc/xdg
  set --global --export XDG_DATA_HOME $HOME/.local/share
  set --global --export --path XDG_DATA_DIRS /usr/local/share/:/usr/share/
  set --global --export XDG_STATE_HOME $HOME/.local/state
  set --global --export XDG_CACHE_HOME $HOME/.cache
  set --global --export XDG_RUNTIME_DIR /run/user/(id -u $USER)
  set --global --export HOMEBREW_PREFIX (brew --prefix)
end

# set path if interactive
if status is-interactive
  set --global --export GOPATH $HOME/go
  set --global --export GOBIN $GOPATH/bin
  set LUAROCKSBIN $HOME/.luarocks/bin
  set PERL5BIN $HOME/perl5/bin
  set SYSGOBIN /usr/local/go/bin
  set RBENVBIN $HOME/.rbenv/bin
  set USRLOCALBIN /usr/local/bin
  set LOCALBIN $HOME/.local/bin
  set OPENJDKBIIN /opt/homebrew/opt/openjdk/bin

  set -l pathdirs $LOCALBIN $USRLOCALBIN $GOBIN $SYSGOBIN $PERL5BIN $LUAROCKSBIN
  for dir in $pathdirs
    if not contains -- $dir $PATH
      fish_add_path --prepend $dir
    end
  end

  set -l appendpathdirs $OPENJDKBIN
  for difr in $appendpathdirs
    if not contains -- $dir $PATH
      fish_add_path --append $dir
    end
  end

  set --global --export PYENV_ROOT $HOME/.pyenv
  set PYENVBIN $PYENV_ROOT/bin
  if set -l index (contains -i PYENV_BIN $fish_user_paths)
    set -e fish_user_paths[$index]
  end

  fish_add_path --prepend $PYENV_BIN

  # issue with path persisting in fish 3.0, this is a roundabout way
  # to ensure virtualenv is always at head of PATH
  if set -q VIRTUAL_ENV
    set VENV_BIN $VIRTUAL_ENV/bin

    # remove existing venv bin from path
    if set -l index (contains -i $VENV_BIN $PATH)
      set -e PATH[$index]
    end

    # and add to beginning of path
    set --export PATH $VENV_BIN $PATH
  end
end
