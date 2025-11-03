foreach localfile (/etc/zsh/local ~/.zsh_local ~/.zshrc.local ~/.zshrc.$HOST ~/.zshrc.$USER); do
    if [[ -r $localfile ]]; then; echo "Sourcing $localfile"; source $localfile; fi
done
