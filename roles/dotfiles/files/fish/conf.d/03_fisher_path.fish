if status is-interactive
    # set fisher path
    set fisher_path $__fish_config_dir/fisher
    ! set --query fisher_path[1] || test "$fisher_path" = $__fish_config_dir && exit

    # add fisher completions, functions, and source conf.d files
    set fish_complete_path $fish_complete_path[1] $fisher_path/completions $fish_complete_path[2..]
    set fish_function_path $fish_function_path[1] $fisher_path/functions $fish_function_path[2..]

    for file in $fisher_path/conf.d/*.fish
        source $file
    end
end
