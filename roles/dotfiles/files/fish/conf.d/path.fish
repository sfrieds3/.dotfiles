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
