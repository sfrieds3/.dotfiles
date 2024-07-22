function wezup --description "Brew update wezterm nightly"
    brew upgrade --cask wezterm@nightly --no-quarantine --greedy-latest
end
