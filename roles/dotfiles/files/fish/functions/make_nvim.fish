function make_nvim --argument _j --description "Make nvim from soure"
    set -q _j[1]; or set -l _j 4
    make -j $_j CMAKE_INSTALL_PREFIX=$HOME/bin/nvim.build install CMAKE_BUILD_TYPE=Release
end
