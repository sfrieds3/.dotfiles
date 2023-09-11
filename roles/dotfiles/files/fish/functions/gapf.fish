function gapf -d "git add patch file"
    git ls-files -m --exclude-standard | fzf --print0 -m | xargs -0 -t -o git add --patch
end
