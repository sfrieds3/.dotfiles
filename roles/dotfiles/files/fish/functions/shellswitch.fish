function shellswitch -d "Switch shells [bash|zsh|fish]"
	chsh -s (brew --prefix)/bin/$argv
end
