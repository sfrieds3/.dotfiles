function make_emacs --argument _j --description "Make nvim from source"
    set -q _j[1]; or set -l _j 8
    ./configure --with-native-comp --with-poll
    make
    ln -s ./src/emacs ~/.local/bin/emacs
end
