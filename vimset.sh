#!/bin/bash

# vim packages to install

vimdir=${HOME}/.vim
vimbundledir=${vimdir}/bundle

# install pathogen
mkdir -p ~/.vim/autoload ~/.vim/bundle && \
    curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim

echo "Cloning vim-syntastic into ${vimbundledir}/vim-syntastic"
git clone https://github.com/vim-syntastic/syntastic.git ${vimbundledir}/vim-syntastic
echo "Cloning tagbar into ${vimbundledir}/tagbar"
git clone https://github.com/majutsushi/tagbar.git ${vimbundledir}/tagbar
echo "Cloning vim-code-dark into ${vimbundledir}/vim-code-dark"
git clone https://github.com/tomasiser/vim-code-dark.git ${vimbundledir}/vim-code-dark
echo "Cloning vim-fugitive into ${vimbundledir}/vim-fugitive"
git clone https://github.com/tpope/vim-fugitive.git ${vimbundledir}/vim-fugitive
echo "Cloning vim-signify into ${vimbundledir}/vim-signify"
git clone https://github.com/mhinz/vim-signify.git ${vimbundledir}/vim-signify
echo "Cloning vim-rails into ${vimbundledir}/vim-rails"
git clone https://github.com/tpope/vim-rails.git ${vimbundledir}/vim-rails
echo "Cloning vim-surround into ${vimbundledir}/vim-surround"
git clone https://github.com/tpope/vim-surround.git ${vimbundledir}/vim-surround
echo "Cloning vlime into ${vimbundledir}/vlime"
git clone https://github.com/vlime/vlime.git ${vimbundledir}/vlime
echo "Cloning jedi-vim into ${vimbundledir}/jedi-vim"
git clone --recursive https://github.com/davidhalter/jedi-vim.git ${vimbundledir}/jedi-vim
echo "Cloning vim-obsession into ${vimbundledir}/vim-obsession"
git clone https://github.com/tpope/vim-obsession.git ${vimbundledir}/vim-obsession
