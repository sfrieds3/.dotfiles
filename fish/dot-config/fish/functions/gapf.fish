function gapf -d "git add patch file"
    git ls-files --modified --deleted --exclude-standard --deduplicate | fzf --print0 -m | xargs -0 -t -o git add --patch
end
