function make_helix -d "Install helix from source"
    cargo install --root ~/.local/bin/helix --path helix-term --locked
end
