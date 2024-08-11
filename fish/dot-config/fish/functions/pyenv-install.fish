function pyenv-install --argument _v --description "Install python version using pyenv and homebrew libs"
    set -q _v[1]; or echo "Please provide python version to install"
    echo "CFLAGS="-I$(brew --prefix openssl)/include" LDFLAGS="-L$(brew --prefix openssl)/lib" pyenv install --verbose $_v"
    CFLAGS="-I$(brew --prefix openssl)/include" LDFLAGS="-L$(brew --prefix openssl)/lib" pyenv install --verbose $_v
end
