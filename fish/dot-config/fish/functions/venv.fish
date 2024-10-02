function venv --description "Activate virtual environment in the current project, or create one if one does not exist"
    set venv_dirs ".venv" venv

    # Function to check if a directory exists
    function dir_exists
        test -d $argv[1]
    end

    # Get the current directory
    set current_dir (pwd)

    # Get the Git root directory
    set git_root (git rev-parse --show-toplevel 2>/dev/null)

    # Check for virtual environment in the current directory
    for venv_dir in $venv_dirs
        if dir_exists "$current_dir/$venv_dir"
            echo "Activating virtual environment in $current_dir/$venv_dir"
            source "$current_dir/$venv_dir/bin/activate.fish"
            return
        end
    end

    # Check for virtual environment in the Git project root
    if test -n "$git_root"
        for venv_dir in $venv_dirs
            if dir_exists "$git_root/$venv_dir"
                echo "Activating virtual environment in $git_root/$venv_dir"
                source "$git_root/$venv_dir/bin/activate.fish"
                return
            end
        end
    end

    # If no virtual environment found, call uv venv
    echo "No virtual environment found, calling uv venv"
    uv venv
end
