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
    set -q KREW_ROOT; and set KREWBIN $KREW_ROOT/.krew/bin; or set KREWBIN $HOME/.krew/bin

    # set -l pathdirs $BREWBIN $BREWSBIN $LOCALBIN $USRLOCALBIN $GOBIN $SYSGOBIN $PERL5BIN $LUAROCKSBIN
    set -l pathdirs $LUAROCKSBIN $PERL5BIN $SYSGOBIN $GOBIN $USRLOCALBIN $LOCALBIN $BREWSBIN $BREWBIN $KREWBIN
    for dir in $pathdirs
        fish_add_path --path --prepend --move $dir
    end

    set -l appendpathdirs $OPENJDKBIN
    for difr in $appendpathdirs
        fish_add_path --path --append --move $dir
    end

    # remove asdf shims so they can be added at beginning of path
    set --local asdf_shims $HOME/.asdf/shims
    if set --local index (contains -i $asdf_shims $PATH)
        set --erase PATH[$index]
    end
end
