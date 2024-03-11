if status is-login
    # load local config from ~/.fish_local, if available
    set -l local_config "$HOME/.fish_local.fish"
    if test -e $local_config
        source $local_config
    end
end

if status is-interactive
    set --global --export fish_prompt_pwd_dir_length 3
    set --global --export fish_prompt_pwd_full_dirs 3

    # atuin
    set --global --export ATUIN_NOBIND true
    atuin init fish | source
    bind \ct _atuin_search

    # zoxide
    zoxide init fish | source

    # asdf
    source (brew --prefix asdf)/libexec/asdf.fish

    if test -f $HOMEBREW_PREFIX/Caskroom/miniconda/base/bin/conda
        eval $HOMEBREW_PREFIX/Caskroom/miniconda/base/bin/conda "shell.fish" hook $argv | source
    end

    # do not add conda env to prompt
    function __conda_add_prompt
    end

    # load kubectl completions
    kubectl completion fish | source

    # configure fzf bindings
    fzf_configure_bindings --history=\co --directory=\eff --git_log=\efl --processes=\efp --variables=\efv

    # load fzf_git
    source $__fish_config_dir/custom/git_fzf.fish
    git_fzf_configure_bindings

    set --global --export JAVA_HOME (/usr/libexec/java_home -v17)

    # load local config from ~/.fish_local, if available
    set -l local_config "$HOME/.fish_local.fish"
    if test -e $local_config
        source $local_config
    end

    fish_set_virtual_env
end
