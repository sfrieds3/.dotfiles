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
	set PYTHON3_VENV_BIN $HOME/.venv/venv/bin
	set OPENJDKBIIN /opt/homebrew/opt/openjdk/bin
	
	set -l pathdirs $LOCALBIN $USRLOCALBIN $GOBIN $SYSGOBIN $PERL5BIN $RBENVBIN $LUAROCKSBIN
	for dir in $pathdirs
	  if not contains -- $dir $PATH
	    fish_add_path $dir
	  end
	end
	
	set -l appendpathdirs $PYTHON3_VENV_BIN $OPENJDKBIN
	for difr in $appendpathdirs
	  if not contains -- $dir $PATH
	    fish_add_path --append $dir
	  end
	end
end
