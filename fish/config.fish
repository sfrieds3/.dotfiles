if status is-interactive
    # Commands to run in interactive sessions can go here
end

#fish_add_path ~/local/bin
contains $HOME/local/bin $fish_user_paths; or set -U fish_user_paths $HOME/local/bin
#fish_add_path ~/lib
contains $HOME/lib $fish_user_paths; or set -U fish_user_paths $HOME/lib
#fish_add_path ~/bin
contains $HOME/bin $fish_user_paths; or set -U fish_user_paths $HOME/bin

# aliases
alias fd="fdfind"

starship init fish | source
