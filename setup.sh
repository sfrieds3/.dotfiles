#!/bin/bash

# update git color for untracked files to white
git config --global color.status.untracked white
git config --global color.status.changed "white normal dim"
git config --global color.status.nobranch "red bold ul"

# update git color for diff
git config --global color.diff.old "yellow reverse dim"

# directory colors for ls
LS_COLORS='ow=01;36;40'
export LS_COLORS

# go get -u github.com/jstemmer/gotags
# go get -u github.com/nsf/gocode
# go get github.com/rogpeppe/godef
# go get -u github.com/derekparker/delve/cmd/dlv
# go get -u github.com/sourcegraph/go-langserver
# gometalinter --install
# export PATH=$PATH:$(go env GOPATH)/bin

# cargo install racer

# apt install packages for ubuntu
sudo apt install exuberant-ctags
sudo apt install leiningen
sudo apt install neovim
sudo apt install python-neovim
sudo apt install python3-neovim
sudo apt install postgresql
sudo apt install tmux
sudo apt install mysql-server
sudo apt install libmysqlclient-dev
sudo apt install rvm

# python pip installation
python3 -m pip install jedi
python3 -m pip install neovim
python -m pip install neovim
python3 -m pip install websocket-client sexpdata
python3 -m pip install virtualenv

# add vimrc and tmux.conf
ln -s ~/.dotfiles/vimrc_basic ~/.vimrc
ln -s ~/.dotfiles/tmux_basic.conf ~/.tmux.conf

# install vim colorschemes
mkdir -p ~/.vim/bundle
mkdir -p ~/.vim/colors
git clone https://github.com/chriskempson/base16-vim.git ~/.vim/bundle
cp -s ~/.vim/bundle/base16/colors/*.vim ~/.vim/colors/
