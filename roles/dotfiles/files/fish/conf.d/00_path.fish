switch (uname)
    case Darwin
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
    set BREWSBIN /opt/homebrew/sbin
    set BREWBIN /opt/homebrew/bin

    set -l pathdirs $BREWBIN $BREWSBIN $LOCALBIN $USRLOCALBIN $GOBIN $SYSGOBIN $PERL5BIN $LUAROCKSBIN
    set -l pathdirs $LUAROCKSBIN $PERL5BIN $SYSGOBIN $GOBIN $USRLOCALBIN $LOCALBIN $BREWSBIN $BREWBIN
    for dir in $pathdirs
        fish_add_path --prepend --move $dir
    end

    set -l appendpathdirs $OPENJDKBIN
    for difr in $appendpathdirs
        fish_add_path --append --move $dir
    end

    set --global --export PYENV_ROOT $HOME/.pyenv
    set PYENVBIN $PYENV_ROOT/bin
    if set -l index (contains -i PYENV_BIN $fish_user_paths)
        set -e fish_user_paths[$index]
    end

    fish_add_path --prepend --move $PYENV_BIN
end
