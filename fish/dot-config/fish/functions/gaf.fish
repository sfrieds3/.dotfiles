function gaf -d "git add file"
    git ls-files --modified --deleted --other --exclude-standard --deduplicate | fzf --print0 -m | xargs -0 -t -o git add
end
