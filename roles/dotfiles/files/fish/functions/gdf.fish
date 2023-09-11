function gdf -d "git diff file"
    git ls-files -m -o --exclude-standard | fzf-tmux -p | xargs git diff
end
