function gcpf -d "git checkout patch file"
    git ls-files -m -o --exclude-standard | fzf --print0 -m | xargs -0 -t -o git checkout --patch
end
